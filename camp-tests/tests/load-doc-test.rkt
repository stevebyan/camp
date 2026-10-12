#lang racket/base

;; Tests for load-doc and live-reload bytecode handling: page modules must
;; reload after their source changes within a single long-running process
;; (see camp-app live rebuilds), while one-shot builds keep using compiled
;; bytecode. Live sessions cache bytecode in a private "camp-live" mode dir
;; so later sessions skip recompiling unchanged sources; raco-built bytecode
;; is never loaded (it forbids redeclaration) but also never deleted.

(require rackunit
         compiler/cm
         compiler/compilation-path
         racket/file
         racket/list
         racket/logging
         racket/path
         racket/system
         setup/dirs
         punct/fetch
         (only-in camp/private/collections load-doc)
         (only-in camp/private/log camp-logger)
         (only-in camp/private/rerequire live-reload? site-cache-root! rerequire!))

;; Edits within the same real-time second would evade rerequire's mtime
;; checks, while future-dated sources are refused by the compilation manager.
;; A virtual clock gives every write a distinct mtime safely in the past.
(define virtual-now (- (current-seconds) 1000))
(define (touch-next! p)
  (set! virtual-now (add1 virtual-now))
  (file-or-directory-modify-seconds p virtual-now))

(define (write-mod! p content)
  (display-to-file content p #:exists 'replace)
  (touch-next! p))

(define (write-page! p body)
  (write-mod! p (format "#lang punct\n---\ntitle: Test Page\ndate: 2026-01-01\n---\n\n~a\n" body)))

(define (write-helper! h greeting)
  (write-mod! h (format "#lang racket/base\n(provide greeting)\n(define (greeting) ~s)\n" greeting)))

(define (write-dep-page! p body [dep "helper.rkt"])
  (write-mod! p (format "#lang punct ~s\n---\ntitle: Dep Page\ndate: 2026-01-01\n---\n\n~a\n" dep body)))

(define (doc-body-string p)
  (format "~a" (load-doc p)))

;; Compile in a subprocess, as raco setup would: compiling in this process
;; would load the module's declaration here and defeat the point of the tests
(define (raco-make! p)
  (check-true (system* (build-path (find-console-bin-dir) "raco") "make" (path->string p))))

;; The live cache for src lives in a "camp-live" mode dir next to it,
;; possibly rerooted by current-compiled-file-roots.
(define (live-zo src)
  (define-values (dir name _dir?) (split-path src))
  (define zo-name (path-add-extension name #".zo" #"_"))
  (for/first ([p (in-directory dir)]
              #:when (and (equal? (file-name-from-path p) zo-name)
                          (member (string->path "camp-live") (explode-path p))))
    p))

(define (live-zo-exists? src)
  (and (live-zo src) #t))

;; ---------------------------------------------------------------------------
;; Outside live-reload mode (one-shot builds), loading must not discard
;; compiled bytecode — that is the compile cache for raco camp build.

(define keep-dir (make-temporary-directory "load-doc-test-keep-~a"))
(define keep-page (build-path keep-dir "page.md.rkt"))
(write-page! keep-page "CACHED PAGE")
(managed-compile-zo keep-page)
(define keep-zo (get-compilation-bytecode-file keep-page))
(check-true (file-exists? keep-zo))
(check-regexp-match #rx"CACHED PAGE" (doc-body-string keep-page))
(check-true (file-exists? keep-zo) "one-shot loads must keep compiled bytecode")
(check-false (live-zo-exists? keep-page) "one-shot loads must not write the live cache")
(delete-directory/files keep-dir)

;; One-shot loads compile missing or stale bytecode for the site's modules,
;; so later builds skip expanding them again, but leave modules outside the
;; cache boundary alone. A source touched after compiling (by a checkout, say)
;; is stale even when unchanged.

(define oneshot-dir (make-temporary-directory "load-doc-test-oneshot-~a"))
(define oneshot-lib-dir (make-temporary-directory "load-doc-test-oneshot-lib-~a"))
(define oneshot-lib (build-path oneshot-lib-dir "outlib.rkt"))
(define oneshot-helper (build-path oneshot-dir "helper.rkt"))
(define oneshot-page (build-path oneshot-dir "page.md.rkt"))
(define touched-page (build-path oneshot-dir "touched.md.rkt"))

(write-mod! oneshot-lib
            "#lang racket/base\n(provide twice)\n(define (twice s) (string-append s s))\n")
(write-mod! oneshot-helper
            (format "#lang racket/base\n(require (file ~s))\n(provide greeting)\n(define (greeting) (twice \"ONE-SHOT \"))\n"
                    (path->string oneshot-lib)))
(write-dep-page! oneshot-page "•greeting[]")
(write-page! touched-page "TOUCHED PAGE")
(raco-make! touched-page)
(file-or-directory-modify-seconds (get-compilation-bytecode-file touched-page) (- virtual-now 500))

(define (zo-fresh? src)
  (define zo (get-compilation-bytecode-file src))
  (and (file-exists? zo)
       (>= (file-or-directory-modify-seconds zo) (file-or-directory-modify-seconds src))))

(site-cache-root! oneshot-dir)
(check-false (zo-fresh? touched-page))
(check-regexp-match #rx"ONE-SHOT ONE-SHOT" (doc-body-string oneshot-page))
(check-regexp-match #rx"TOUCHED PAGE" (doc-body-string touched-page))
(check-true (zo-fresh? oneshot-page) "one-shot loads must compile site sources")
(check-true (zo-fresh? oneshot-helper) "one-shot loads must compile in-site dependencies")
(check-true (zo-fresh? touched-page) "one-shot loads must refresh stale bytecode")
(check-false (file-exists? (get-compilation-bytecode-file oneshot-lib))
             "one-shot loads must not compile modules outside the boundary")
(check-false (live-zo-exists? oneshot-page) "one-shot loads must not write the live cache")
(delete-directory/files oneshot-dir)
(delete-directory/files oneshot-lib-dir)

;; Everything below exercises live-reload mode (GUI app, raco camp serve)
(live-reload? #t)

;; ---------------------------------------------------------------------------
;; load-doc picks up source edits across repeated loads

(define page-a (make-temporary-file "load-doc-test-a-~a.md.rkt"))
(write-page! page-a "VERSION ONE")
(check-regexp-match #rx"VERSION ONE" (doc-body-string page-a))

(write-page! page-a "VERSION TWO")
(check-regexp-match #rx"VERSION TWO" (doc-body-string page-a))
(delete-file page-a)

;; ---------------------------------------------------------------------------
;; A plain get-doc AFTER a load-doc must not pin the module to a stale
;; version. (The reverse order would: rerequire never reloads a module it
;; did not load first, which is why all camp doc loading goes via load-doc.)

(define page-b (make-temporary-file "load-doc-test-b-~a.md.rkt"))
(write-page! page-b "FIRST DRAFT")
(check-regexp-match #rx"FIRST DRAFT" (doc-body-string page-b))
(check-regexp-match #rx"FIRST DRAFT" (format "~a" (get-doc page-b)))

(write-page! page-b "SECOND DRAFT")
(check-regexp-match #rx"SECOND DRAFT" (doc-body-string page-b))
(check-regexp-match #rx"SECOND DRAFT" (format "~a" (get-doc page-b)))
(delete-file page-b)

;; ---------------------------------------------------------------------------
;; Editing a module a page depends on must reload the page, even when the
;; page source itself is untouched. (Site authors edit root main.rkt to add
;; functions for use in page sources; rerequire walks recorded dependencies.)

(define dep-dir (make-temporary-directory "load-doc-test-deps-~a"))
(define helper (build-path dep-dir "helper.rkt"))
(define dep-page (build-path dep-dir "dep-page.md.rkt"))

(write-helper! helper "HELLO FROM V1")
(write-dep-page! dep-page "•greeting[]")

(site-cache-root! dep-dir)
(check-regexp-match #rx"HELLO FROM V1" (doc-body-string dep-page))

(write-helper! helper "HELLO FROM V2")
(write-dep-page! dep-page "•greeting[] (again)") ; expansion declares the helper in the compilation namespace
(check-regexp-match #rx"HELLO FROM V2" (doc-body-string dep-page))

;; Compile-time staleness: a dependency edit that adds an export, used by an
;; edited page, must not expand the page against a stale declaration of the
;; dependency lingering in the compilation namespace from earlier builds.
(write-mod! helper
            (string-append "#lang racket/base\n(provide greeting shout)\n"
                           "(define (greeting) \"HELLO FROM V3\")\n"
                           "(define (shout) \"NEW EXPORT\")\n"))
(write-dep-page! dep-page "•greeting[] •shout[]")
(check-regexp-match #rx"HELLO FROM V3.*NEW EXPORT" (doc-body-string dep-page))

;; Reloads are reported through the camp logger, relative to the cache root,
;; and never on stderr; first loads and unchanged modules are not reported.
(define (load-doc-report p)
  (define msgs '())
  (define err (open-output-string))
  (parameterize ([current-error-port err])
    (with-intercepted-logging
      (λ (v) (set! msgs (cons (vector-ref v 1) msgs)))
      (λ () (load-doc p))
      #:logger camp-logger 'info 'camp))
  (list (reverse msgs) (get-output-string err)))

(define fresh-page (build-path dep-dir "fresh-page.md.rkt"))
(write-dep-page! fresh-page "•greeting[]")
(check-equal? (load-doc-report fresh-page) '(() ""))
(check-equal? (load-doc-report dep-page) '(() ""))

(write-mod! helper
            (string-append "#lang racket/base\n(provide greeting shout)\n"
                           "(define (greeting) \"HELLO FROM V4\")\n"
                           "(define (shout) \"NEW EXPORT\")\n"))
(let ([report (load-doc-report dep-page)])
  (check-equal? (length (car report)) 2)
  (check-regexp-match #rx"Reloaded.* helper\\.rkt$" (car (car report)))
  (check-regexp-match #rx"Reloaded.* dep-page\\.md\\.rkt$" (cadr (car report)))
  (check-equal? (cadr report) ""))
(delete-directory/files dep-dir)

;; ---------------------------------------------------------------------------
;; An edited dependency must reach every dependent, although each is loaded
;; by a call of its own and the dependency is already current again by the
;; second: pages sharing a helper, a page reaching it through a module no
;; earlier call visited, and pages loaded after a render module that shares
;; it (the order the file watcher uses).

(define share-dir (make-temporary-directory "load-doc-test-share-~a"))
(define share-helper (build-path share-dir "helper.rkt"))
(define share-mid (build-path share-dir "mid.rkt"))
(define share-render (build-path share-dir "render.rkt"))
(define share-pages
  (for/list ([name (in-list '("a.md.rkt" "b.md.rkt" "c.md.rkt"))])
    (build-path share-dir name)))

(write-helper! share-helper "SHARED V1")
(write-mod! share-mid
            "#lang racket/base\n(require \"helper.rkt\")\n(provide greeting)\n")
(write-mod! share-render
            "#lang racket/base\n(require \"helper.rkt\")\n(provide render)\n(define (render) (greeting))\n")
(for ([page (in-list share-pages)]
      [dep (in-list '("helper.rkt" "helper.rkt" "mid.rkt"))])
  (write-dep-page! page "•greeting[]" dep))

(define (share-render-string)
  (rerequire! share-render)
  ((dynamic-require share-render 'render)))

(site-cache-root! share-dir)
(check-equal? (share-render-string) "SHARED V1")
(for ([page (in-list share-pages)])
  (check-regexp-match #rx"SHARED V1" (doc-body-string page)))

(write-helper! share-helper "SHARED V2")
(check-equal? (share-render-string) "SHARED V2")
(for ([page (in-list share-pages)])
  (check-regexp-match #rx"SHARED V2" (doc-body-string page) (path->string page)))

(write-helper! share-helper "SHARED V3")
(for ([page (in-list share-pages)])
  (check-regexp-match #rx"SHARED V3" (doc-body-string page) (path->string page)))
(check-equal? (share-render-string) "SHARED V3")

;; A page that requires another page is a dependent like any other
(define quoted-page (build-path share-dir "quoted.md.rkt"))
(define quoting-page (build-path share-dir "quoting.md.rkt"))
(write-page! quoted-page "QUOTED ONE")
(write-page! quoting-page
             "•(require (prefix-in q: \"quoted.md.rkt\"))\n\n•(format \"~a\" q:doc)")
(check-regexp-match #rx"QUOTED ONE" (doc-body-string quoted-page))
(check-regexp-match #rx"QUOTED ONE" (doc-body-string quoting-page))

(write-page! quoted-page "QUOTED TWO")
(check-regexp-match #rx"QUOTED TWO" (doc-body-string quoted-page))
(check-regexp-match #rx"QUOTED TWO" (doc-body-string quoting-page))
(delete-directory/files share-dir)

;; ---------------------------------------------------------------------------
;; A page with raco-compiled bytecode must still be reloadable: that bytecode
;; is compiled with constant enforcement, so a module first declared from it
;; can never be redeclared ("cannot re-define a constant"). Live loads must
;; ignore it (loading via the camp-live cache instead) — but not delete it,
;; since it is the CLI's compile cache.

(define zo-dir (make-temporary-directory "load-doc-test-zo-~a"))
(define page-c (build-path zo-dir "compiled-page.md.rkt"))
(write-page! page-c "COMPILED ONE")
(managed-compile-zo page-c)
(define page-c-zo (get-compilation-bytecode-file page-c))

(site-cache-root! zo-dir)
(check-regexp-match #rx"COMPILED ONE" (doc-body-string page-c))

(write-page! page-c "COMPILED TWO")
(check-regexp-match #rx"COMPILED TWO" (doc-body-string page-c))
(check-true (file-exists? page-c-zo) "live loads must not delete raco-built bytecode")
(delete-directory/files zo-dir)

;; ---------------------------------------------------------------------------
;; Same constraint, one level down: a raco-compiled DEPENDENCY of a page
;; (e.g. the site's main.rkt) is loaded by rerequire's own handler, which
;; load-doc cannot intercept per-module. The camp-live cache mode keeps
;; that handler blind to raco-built bytecode, so those first declarations
;; also avoid constant enforcement.

(define cdep-dir (make-temporary-directory "load-doc-test-cdep-~a"))
(define chelper (build-path cdep-dir "helper.rkt"))
(define cdep-page (build-path cdep-dir "dep-page.md.rkt"))

(write-helper! chelper "COMPILED DEP V1")
(write-dep-page! cdep-page "•greeting[]")
(raco-make! cdep-page)

(site-cache-root! cdep-dir)
(check-regexp-match #rx"COMPILED DEP V1" (doc-body-string cdep-page))

(write-helper! chelper "COMPILED DEP V2")
(check-regexp-match #rx"COMPILED DEP V2" (doc-body-string cdep-page))
(delete-directory/files cdep-dir)

;; ---------------------------------------------------------------------------
;; With the cache boundary set (load-site does this), live loads write
;; camp-live bytecode for the page and its in-site dependencies — the warm
;; cache for later sessions — but never for modules outside the boundary.

(define site-dir (make-temporary-directory "load-doc-test-site-~a"))
(define lib-dir (make-temporary-directory "load-doc-test-lib-~a"))
(define site-helper (build-path site-dir "helper.rkt"))
(define outside-lib (build-path lib-dir "outlib.rkt"))
(define site-page (build-path site-dir "cached-page.md.rkt"))

(write-mod! outside-lib
            "#lang racket/base\n(provide twice)\n(define (twice s) (string-append s s))\n")
(write-mod! site-helper
            (format "#lang racket/base\n(require (file ~s))\n(provide greeting)\n(define (greeting) (twice \"IN-SITE \"))\n"
                    (path->string outside-lib)))
(write-dep-page! site-page "•greeting[]")

(site-cache-root! site-dir)
(check-regexp-match #rx"IN-SITE IN-SITE" (doc-body-string site-page))
(check-true (live-zo-exists? site-page) "live loads must cache page bytecode")
(check-true (live-zo-exists? site-helper) "live loads must cache in-site dependencies")
(check-false (live-zo-exists? outside-lib) "live cache must not extend outside the boundary")
(delete-directory/files site-dir)
(delete-directory/files lib-dir)

;; ---------------------------------------------------------------------------
;; Cached site bytecode links against the installed bytecode of the libraries
;; it imports, so it must be rebuilt when that bytecode changes even though
;; the library's own source has not: after a Racket upgrade, or a raco setup
;; of a library whose dependency was edited. (A reexported binding links
;; straight to the defining module's instance; a stale cache then fails to
;; instantiate with "reference to a variable that is not exported".) A
;; running session keeps the library instance it first declared, but the
;; cache it writes from then on must already suit the next session.

(define (load-doc-in-new-session site page)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (check-true (system* (build-path (find-console-bin-dir) "racket")
                         "-l" "racket/base" "-l" "camp/private/rerequire" "-l" "camp/private/collections"
                         "-e" (format "(live-reload? #t) (site-cache-root! ~s) (display (load-doc (string->path ~s)))"
                                      (path->string site) (path->string page)))))
  (get-output-string out))

(define relink-site (make-temporary-directory "load-doc-test-relink-site-~a"))
(define relink-lib (make-temporary-directory "load-doc-test-relink-lib-~a"))
(define relink-dep (build-path relink-lib "outdep.rkt"))
(define relink-out (build-path relink-lib "outlib.rkt"))
(define relink-helper (build-path relink-site "helper.rkt"))
(define relink-page (build-path relink-site "relink-page.md.rkt"))

(define (write-relink-dep! sep)
  (write-mod! relink-dep
              (format "#lang racket/base\n(provide twice)\n(define (twice s) (string-append s ~s s))\n" sep)))

(write-relink-dep! "")
(write-mod! relink-out
            (format "#lang racket/base\n(require (file ~s))\n(provide twice)\n" (path->string relink-dep)))
(write-mod! relink-helper
            (format "#lang racket/base\n(require (file ~s))\n(provide greeting)\n(define (greeting) (twice \"RELINK\"))\n"
                    (path->string relink-out)))
(write-dep-page! relink-page "•greeting[]")
(raco-make! relink-out)

(site-cache-root! relink-site)
(check-regexp-match #rx"RELINKRELINK" (doc-body-string relink-page))
(define relink-helper-dep (path-replace-extension (live-zo relink-helper) #".dep"))
(define relink-recorded (file->string relink-helper-dep))
;; Written this second, the cache would tie with the library bytecode compiled next
(for ([f (list (live-zo relink-helper) relink-helper-dep)])
  (file-or-directory-modify-seconds f (- (current-seconds) 10)))

(write-relink-dep! "+")
;; From scratch: cm itself misses a dependency rebuilt within the same second
(delete-directory/files (build-path relink-lib "compiled"))
(raco-make! relink-out)
(check-not-exn (λ () (doc-body-string relink-page)) "a running session must survive a library recompile")
(check-regexp-match #rx"RELINK\\+RELINK" (load-doc-in-new-session relink-site relink-page))
(check-not-equal? (file->string relink-helper-dep) relink-recorded
                  "recompiled library bytecode must rebuild the cache of its dependents")
(delete-directory/files relink-site)
(delete-directory/files relink-lib)

(live-reload? #f)
