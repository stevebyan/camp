#lang racket/base

(require compiler/cm
         compiler/compilation-path
         racket/list
         racket/path
         racket/promise
         syntax/modresolve
         "log.rkt")

(provide live-reload?
         site-cache-root!
         rerequire!)

;; Bytecode produced by raco setup/make is compiled with constant enforcement,
;; so a module first declared in a process from its .zo can never be
;; redeclared ("cannot re-define a constant"). In a live-reload process (the
;; GUI app, raco camp serve) site modules therefore live in their own bytecode
;; world: a "camp-live" mode dir, compiled here without constant enforcement
;; and declared from there by rerequire!, never by the regular loader. The
;; cache persists across sessions, so only changed sources ever recompile.
;; All loading of site modules must go through rerequire! (or
;; load-doc/load-site, which use it). Outside live-reload mode it only
;; brings the regular bytecode of site modules up to date, as raco make
;; would, and the caller's own dynamic-require loads as usual.
;;
;; racket/rerequire is not used: it forgets what it reloaded from one call to
;; the next, so of several dependents of an edited module, each loaded by a
;; call of its own, only the first would be reloaded.

(define live-reload? (make-parameter #f))

(define live-mode (build-path "compiled" "camp-live"))

;; The cache boundary, normally the site root (set by load-site). Modules
;; outside it are treated as stable libraries; they are never compiled into
;; the cache, never tracked for reload, and must always enter the live
;; namespace from their installed bytecode.
(define cache-root (box #f))
(define (site-cache-root! root)
  (set-box! cache-root (path->directory-path (simple-form-path root))))

;; split-path returns parent dirs in trailing-slash form, matching cache-root
(define (in-cache? p)
  (define root (unbox cache-root))
  (and root
       (let loop ([p p])
         (or (equal? root p)
             (let-values ([(base _name _dir?) (split-path p)])
               (and (path? base) (loop base)))))))

;; Report a freshness stamp for out-of-boundary modules so the compilation
;; manager records them as dependencies without compiling them. Cached site
;; bytecode links against the library instance the regular loader declares,
;; so the stamp follows that form: the installed bytecode, or the source
;; when it is newer. A library recompiled from unchanged source (a Racket
;; upgrade; raco setup after one of its own dependencies was edited) thereby
;; still invalidates its dependents in the cache.
;;
;; A library recompiled during a session cannot be reloaded (the live
;; namespace keeps its first instance), but cache written from then on must
;; still link against the new bytecode its stamp records, so the next
;; session finds it consistent: dependents are compiled in a fresh
;; compilation namespace (see below) rather than against the stale
;; declaration.
(define stamps (make-hash))

(define ((skip-outside entry) path)
  (define p (simple-form-path path))
  (and (not (equal? p entry))
       (not (in-cache? p))
       (let ([stamp (or (file-stamp-in-paths p (list (car (explode-path p)))) ; any path is under its own root
                        (cons -inf.0 ""))]
             [seen (hash-ref stamps p #f)])
         (hash-set! stamps p stamp)
         (when (and seen (live-reload?) (not (equal? (car seen) (car stamp))))
           (log-camp-warning "Recompiled since this session started; restart to use it: ~a" p)
           (raise (exn:stale-namespace)))
         stamp)))

;; Compilation runs in a private namespace: instantiating a module here (for
;; a dependent's expansion) must not declare it in the live namespace, where
;; it would escape rerequire's dependency tracking and never reload. The
;; namespace persists so the language chain instantiates once — but when a
;; module already declared here is recompiled (a shared helper was edited),
;; dependents must not expand against the stale declaration: start over in a
;; fresh namespace, where every declaration is then current.
(define compile-namespace (box (make-base-namespace)))

(struct exn:stale-namespace ())

;; A module declared here is not loaded again when a dependent requires it,
;; so its staleness would pass unnoticed whenever the dependent is compiled
;; outright (its own source changed) rather than after its dependencies:
;; have the compilation manager check such declarations on resolution.
(define (checking-resolver orig)
  (case-lambda
    [(name ns) (orig name ns)]
    [(mod rel stx load?)
     (when load?
       (define name (resolved-module-path-name (orig mod rel stx #f)))
       (define file (if (pair? name) (car name) name))
       (when (path? file)
         (define p (simple-form-path file))
         (when (and (in-cache? p) (module-declared? (make-resolved-module-path p)))
           (managed-compile-zo p))))
     (orig mod rel stx load?)]))

(define (compile-live! path)
  (let retry ()
    (define ns (unbox compile-namespace))
    (with-handlers ([exn:stale-namespace?
                     (λ (_e)
                       (set-box! compile-namespace (make-base-namespace))
                       (retry))])
      (parameterize* ([use-compiled-file-paths (cons live-mode (use-compiled-file-paths))]
                      [current-namespace ns]
                      [current-module-name-resolver (checking-resolver (current-module-name-resolver))]
                      [current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)]
                      [compile-enforce-module-constants #f]
                      [manager-skip-file-handler (skip-outside path)]
                      [manager-compile-notify-handler
                       (λ (p)
                         (define sp (simple-form-path p))
                         (when (and (not (equal? sp path))
                                    (module-declared? (make-resolved-module-path sp)))
                           (raise (exn:stale-namespace))))])
        (managed-compile-zo path)))))

;; ---------------------------------------------------------------------------
;; Loading
;;
;; In-site modules are declared here from the cache, imports first. Cached
;; bytecode carries cross-module references into the installed bytecode of
;; the libraries it was compiled against, so every out-of-boundary import is
;; instead declared through the regular loader, which finds no camp-live
;; bytecode for it and so loads the installed form.
;;
;; A module is declared again when its cached bytecode has changed (the
;; compilation manager rewrites it for any edit the module depends on,
;; included files too) or when an in-site import has been declared since.
;; Declarations are counted for the life of the process, not per call, so
;; every dependent of an edited module follows it however late it is loaded.

(struct decl (stamp imports at))
(define decls (make-hash))
(define libraries (make-hash))
(define declare-count 0)

(define (live-zo-path path)
  (for/or ([root (in-list (current-compiled-file-roots))])
    (define zo (get-compilation-bytecode-file path #:modes (list live-mode) #:roots (list root)))
    (and (file-exists? zo) zo)))

(define (zo-stamp zo)
  (hash-ref (file-or-directory-stat zo) 'modify-time-nanoseconds))

(define (read-zo zo)
  (parameterize ([read-accept-compiled #t])
    (call-with-input-file zo read)))

(define (compiled-imports code)
  (let loop ([c code] [acc '()])
    (define imports (apply append (map cdr (module-compiled-imports c))))
    (for*/fold ([acc (append imports acc)])
               ([non-star? (in-list '(#t #f))]
                [sub (in-list (module-compiled-submodules c non-star?))])
      (loop sub acc))))

(define (module-file r wrt)
  (let base ([r r])
    (cond
      [(path? r) (simple-form-path r)]
      [(and (pair? r) (eq? (car r) 'submod))
       (base (if (path? (cadr r)) (cadr r) wrt))]
      [else #f])))

(define (import-files code path)
  (remove-duplicates
   (filter-map (λ (mpi)
                 (module-file (with-handlers ([exn:fail? (λ (_) #f)])
                                (resolve-module-path-index mpi path))
                              path))
               (compiled-imports code))))

(define (declare-library! path)
  (hash-ref! libraries path
             (λ () (with-handlers ([exn:fail? void])
                     (dynamic-require path (void))))))

(define (declare! path code)
  (define-values (dir _name _dir?) (split-path path))
  (parameterize ([current-module-declare-name (make-resolved-module-path path)]
                 [current-load-relative-directory dir])
    (eval code)))

(define (log-reload! path)
  (log-camp-info "  ~a ~a"
                 (dim "Reloaded")
                 (if (in-cache? path) (find-relative-path (unbox cache-root) path) path)))

;; Returns the count at which path was last declared
(define (refresh! entry)
  (define seen (make-hash))
  (let visit ([path entry])
    (cond
      [(hash-ref seen path #f)]
      [(live-zo-path path)
       => (λ (zo)
            (hash-set! seen path 0) ; a module's submodules import it
            (define stamp (zo-stamp zo))
            (define prev (hash-ref decls path #f))
            (define unchanged? (and prev (= stamp (decl-stamp prev))))
            (define code (delay (read-zo zo)))
            (define imports
              (if unchanged? (decl-imports prev) (import-files (force code) path)))
            (define newest-import
              (for/fold ([newest 0]) ([file (in-list imports)])
                (cond
                  [(in-cache? file) (max newest (visit file))]
                  [else (declare-library! file) newest])))
            (unless (and unchanged? (<= newest-import (decl-at prev)))
              (declare! path (force code))
              (when prev (log-reload! path))
              (set! declare-count (add1 declare-count))
              (hash-set! decls path (decl stamp imports declare-count)))
            (define at (decl-at (hash-ref decls path)))
            (hash-set! seen path at)
            at)]
      [else (declare-library! path) 0])))

;; ---------------------------------------------------------------------------

;; A module already declared cannot be redeclared outside live-reload mode,
;; so its bytecode is not checked again
(define (compile-stale! path)
  (unless (module-declared? (make-resolved-module-path path))
    (parameterize ([manager-skip-file-handler (skip-outside path)])
      (managed-compile-zo path))))

(define (rerequire! mod)
  (define path
    (with-handlers ([exn:fail? (λ (_) #f)])
      (module-file (if (path? mod) mod (resolve-module-path mod #f)) #f)))
  (cond
    [(not path) (void)]
    [(live-reload?)
     (compile-live! path)
     (void (refresh! path))]
    [(in-cache? path) (compile-stale! path)]))
