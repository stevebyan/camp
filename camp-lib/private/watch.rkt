#lang racket/base

;; File system watching for dev server

(require racket/file
         racket/list
         racket/path
         racket/string
         "structs.rkt"
         "collections.rkt")

(provide get-watch-paths
         start-watcher!
         path-change-type)

;; ---------------------------------------------------------------------------
;; Path Change Types

(define (path-change-type changed-path site site-config-path)
  (define simplified (simplify-path changed-path))
  (define config-simplified (simplify-path site-config-path))
  (define root (site-root site))
  (define static-dir (build-path root (site-static-folder site)))

  (cond
    ;; Site config file
    [(equal? simplified config-simplified) 'config]

    ;; Static folder (path starts with static dir)
    [(path-prefix? simplified static-dir) 'static]

    ;; Source file (matches source extension in a collection directory)
    [(is-source-file? simplified site) 'source]

    ;; .rkt file
    [(path-has-extension? simplified #".rkt") 'rkt]

    [else 'unknown]))

(define (path-prefix? path prefix)
  (define path-parts (explode-path (simplify-path path)))
  (define prefix-parts (explode-path (simplify-path prefix)))
  (and (>= (length path-parts) (length prefix-parts))
       (equal? (take path-parts (length prefix-parts)) prefix-parts)))

(define (is-source-file? path site)
  (define source-ext (site-sources site))
  (define root (site-root site))
  (and (is-source? path source-ext)
       (for/or ([coll (in-list (site-collections site))])
         (define source-dir (build-path root (source-pattern->directory (collection-source coll))))
         (path-prefix? path source-dir))))

;; ---------------------------------------------------------------------------
;; Watch Path Enumeration

(define (get-watch-paths site site-config-path)
  (define root (site-root site))
  (define source-ext (site-sources site))
  (define static-dir (build-path root (site-static-folder site)))
  (define output-dir (build-path root (site-output-folder site)))

  (remove-duplicates
   (filter file-exists?
           (append
            (list (simplify-path site-config-path))
            (get-source-paths site root source-ext)
            (get-static-paths static-dir)
            (get-render-module-paths site root)
            (get-root-rkt-files root output-dir)))))

(define (get-source-paths site root source-ext)
  (for*/list ([coll (in-list (site-collections site))]
              [path (in-list (get-collection-source-paths root coll source-ext))])
    path))

(define (get-collection-source-paths root coll source-ext)
  (define source-dir (build-path root (source-pattern->directory (collection-source coll))))
  (if (directory-exists? source-dir)
      (find-sources source-dir source-ext)
      '()))

(define (get-static-paths static-dir)
  (if (directory-exists? static-dir)
      (find-files file-exists? static-dir)
      '()))

(define (get-render-module-paths site root)
  (define render-specs
    (append
     (filter-map collection-render-with (site-collections site))
     (map feed-config-render-with (site-feeds site))
     (if (site-default-render site)
         (list (site-default-render site))
         '())))

  (filter-map
   (λ (spec)
     (define mod-path (car spec))
     (resolve-local-module-path mod-path root))
   render-specs))

(define (resolve-local-module-path mod-path root)
  (cond
    [(symbol? mod-path)
     (define parts (string-split (symbol->string mod-path) "/"))
     (define file-name (string-append (last parts) ".rkt"))
     (define local-path (build-path root file-name))
     (if (file-exists? local-path)
         local-path
         #f)]
    [(string? mod-path)
     (define local-path (build-path root mod-path))
     (if (file-exists? local-path)
         local-path
         #f)]
    [else #f]))

(define (get-root-rkt-files root output-dir)
  (if (directory-exists? root)
      (for/list ([item (in-list (directory-list root #:build? #t))]
                 #:when (and (file-exists? item)
                             (path-has-extension? item #".rkt")
                             ;; Exclude output directory
                             (not (path-prefix? item output-dir))))
        item)
      '()))

;; ---------------------------------------------------------------------------
;; File Watcher
;;
;; The watcher polls modification times. A filesystem change event costs an
;; open file descriptor per path on macOS, where a process gets 256 by
;; default: a site of a few hundred files would leave the server none to
;; accept connections with.

(define (snapshot paths)
  (for/list ([p (in-list paths)])
    (cons p (with-handlers ([exn:fail:filesystem? (λ (_) #f)])
              (hash-ref (file-or-directory-stat p) 'modify-time-nanoseconds)))))

;; The first path, in watch-list order, that was edited or added; failing
;; that, one that was removed
(define (first-change old new)
  (define (differing entries others)
    (define times (make-immutable-hash others))
    (for/first ([entry (in-list entries)]
                #:unless (equal? (cdr entry) (hash-ref times (car entry) #f)))
      (car entry)))
  (or (differing new old) (differing old new)))

;; paths may be a thunk, re-consulted each poll so the watch list can follow
;; new files and site config changes without restarting the watcher. The
;; snapshot compared against is the one taken before on-change ran, so an
;; edit made during a build is reported by the next poll.
(define (start-watcher! paths on-change #:interval-ms [interval-ms 500])
  (define stop (make-semaphore 0))
  (define (watched)
    (snapshot (if (procedure? paths) (paths) paths)))
  ;; taken before returning: an edit made right after must not join the baseline
  (define initial (watched))
  (define watcher
    (thread
     (λ ()
       (let loop ([old initial])
         (unless (sync/timeout (/ interval-ms 1000.0) stop)
           ;; a folder deleted mid-enumeration: look again next time
           (define new (with-handlers ([exn:fail:filesystem? (λ (_) old)])
                         (watched)))
           (define changed (first-change old new))
           (when changed (on-change changed))
           (loop new))))))
  (λ ()
    (semaphore-post stop)
    (void (sync/timeout 0.5 watcher))))
