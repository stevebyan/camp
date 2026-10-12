#lang racket/base

;; Camp CLI - raco camp command dispatcher

(require racket/cmdline
         racket/exn
         racket/file
         racket/format
         racket/list
         racket/lazy-require
         racket/logging
         racket/match
         racket/path
         racket/port
         "rerequire.rkt"
         racket/string
         racket/system
         racket/vector
         "main.rkt"
         "build.rkt"
         "book-build.rkt"
         "watch.rkt"
         "structs.rkt"
         "draft.rkt"
         (only-in "output.rkt" format-duration with-timing count-files-in-directory)
         "log.rkt"
         "new-site.rkt")

;; The web server is loaded only by the serve command
(lazy-require ["serve.rkt" (start-server)])

(provide main)

;; ---------------------------------------------------------------------------
;; Main Entry Point

(define LINE-WIDTH 60)

(define (main)
  (define args (current-command-line-arguments))
  (cond
    [(zero? (vector-length args))
     (show-usage)]
    [else
     (define cmd (vector-ref args 0))
     (define cmd-args (vector-drop args 1))
     (parameterize ([current-command-line-arguments cmd-args])
       (match cmd
         ["build" (with-logging-to-stderr run-build)]
         ["serve" (with-logging-to-stderr run-serve)]
         ["deploy" (with-logging-to-stderr run-deploy)]
         ["print" (with-logging-to-stderr run-print)]
         ["new" (with-logging-to-stderr run-new)]
         ["draft" (with-logging-to-stderr run-draft)]
         ["help" (show-usage)]
         [_ (eprintf "Unknown command: ~a\n" cmd)
            (show-usage)
            (exit 1)]))]))

(define (show-usage)
  (displayln "Usage: raco camp <command> [options]")
  (displayln "")
  (displayln "Commands:")
  (displayln "  build [options] [site]        Build the site")
  (displayln "    --fresh                     Clear output folder before building")
  (displayln "    --verbose, -v               Show detailed output")
  (displayln "    --drama                     Treat warnings as errors (non-zero exit)")
  (displayln "  serve [options] [site]        Start dev server")
  (displayln "    --port N                    Port number (default: 8000)")
  (displayln "    --no-watch                  Disable file watching")
  (displayln "    --apache-log                Use Apache combined log format")
  (displayln "  deploy [site]                 Run deploy script")
  (displayln "  print [options] [book.rkt]    Build book PDF")
  (displayln "    --open                      Open PDF after building")
  (displayln "  draft [collection] [site]     Create new draft post")
  (displayln "  new <name>                    Create new site from template")
  (displayln "  help                          Show this help")
  (displayln "")
  (displayln "The [site] argument can be a path to site.rkt or an installed package name.")
  (displayln "If not specified, looks for site.rkt in current directory."))

(define (resolve-site-spec/cli spec)
  (cond
    [(not spec)
     (if (file-exists? "site.rkt")
         "site.rkt"
         (begin
           (log-camp-error "No site.rkt found in current directory.")
           (log-camp-error "Specify a path or installed package name.")
           (exit 1)))]
    [(resolve-site-spec spec)]
    [else
     (log-camp-error (~a "Not found: " spec))
     (log-camp-error "Provide a path to site.rkt or an installed package name.")
     (exit 1)]))

;; ---------------------------------------------------------------------------
;; Build command

(define (run-build)
  (define fresh? #f)
  (define verbose? #f)
  (define warnings-as-errors? #f)

  (define remaining
    (command-line
     #:program "raco camp build"
     #:argv (current-command-line-arguments)
     #:once-each
     [("--fresh") "Clear output folder before building"
                  (set! fresh? #t)]
     [("--verbose" "-v") "Show detailed output"
                         (set! verbose? #t)]
     [("--drama") "Treat warnings as errors (non-zero exit)"
                 (set! warnings-as-errors? #t)]
     #:args ([site #f])
     site))

  (define resolved-path (resolve-site-spec/cli remaining))

  (define total-start (current-inexact-monotonic-milliseconds))

  (define warnings '())
  (define (collect-warning! vec)
    (set! warnings (cons (vector-ref vec 1) warnings)))

  (define-values (site load-ms)
    (with-timing
      (with-handlers ([exn:fail?
                       (λ (e)
                         (log-error "loading site" (exn-message e))
                         (exit 1))])
        (load-site resolved-path))))

  (when verbose?
    (log-phase-line "Load" (~a "from " resolved-path) load-ms))

  (when fresh?
    (define output-dir (build-path (site-root site) (site-output-folder site)))
    (when (directory-exists? output-dir)
      (delete-directory/files output-dir)
      (when verbose?
        (log-phase-line "Clean" "cleared output folder" #f))))

  (define coll-count (length (site-collections site)))
  (define feed-count (length (site-feeds site)))
  (define static-dir (build-path (site-root site) (site-static-folder site)))
  (define static-count (count-files-in-directory static-dir))

  (define-values (info collect-ms)
    (with-timing
      (with-intercepted-logging
        collect-warning!
        (λ ()
          (with-handlers ([exn:fail?
                           (λ (e)
                             (log-error "collecting" (exn-message e))
                             (exit 1))])
            (collect site)))
        #:logger camp-logger
        'warning)))

  (define page-count (length (site-info-pages info)))
  (log-phase-line "Collect"
                  (format-count-desc page-count "page" coll-count "collection")
                  collect-ms)

  (define-values (_ build-ms)
    (with-timing
      (with-intercepted-logging
        collect-warning!
        (λ ()
          (with-handlers ([exn:fail?
                           (λ (e)
                             (log-error "building" e)
                             (exit 1))])
            (build! site info)))
        #:logger camp-logger
        'warning)))

  (log-phase-line "Build"
                  (pluralize page-count "page")
                  build-ms)

  (when (> feed-count 0)
    (log-phase-line "Feeds"
                    (pluralize feed-count "feed")
                    #f))

  (when (> static-count 0)
    (log-phase-line "Static"
                    (pluralize static-count "file")
                    #f))

  (define warning-count (length warnings))
  (when (> warning-count 0)
    (define root-str (path->string (site-root site)))
    (define root-prefix
      (if (string-suffix? root-str "/")
          root-str
          (string-append root-str "/")))
    (log-camp-info (~a "  " (yellow "⚠") " " (bold (pluralize warning-count "warning")) ":"))
    (for ([w (in-list (reverse warnings))])
      (define display-warning (string-replace w root-prefix ""))
      (log-camp-info (~a "    • " display-warning))))

  (define total-ms (- (current-inexact-monotonic-milliseconds) total-start))
  (if (and warnings-as-errors? (> warning-count 0))
      (begin
        (log-camp-info (~a "  " (red "✗") " " (bold (~a "Failed in " (format-duration total-ms)))
                     " " (dim "(warnings treated as errors)")))
        (exit 1))
      (begin
        (log-camp-info (~a "  " (green "✓") " " (bold (~a "Done in " (format-duration total-ms))))))))

;; ---------------------------------------------------------------------------
;; Serve command

(define (run-serve)
  (define port 8000)
  (define watch? #t)
  (define log-format 'modern)

  (define remaining
    (command-line
     #:program "raco camp serve"
     #:argv (current-command-line-arguments)
     #:once-each
     [("--port") p "Port number (default: 8000)"
                 (define n (string->number p))
                 (unless (and n (exact-positive-integer? n) (<= n 65535))
                   (log-camp-error (~a "Invalid port number: " p))
                   (exit 1))
                 (set! port n)]
     [("--no-watch") "Disable file watching"
                     (set! watch? #f)]
     [("--apache-log") "Use Apache combined log format"
                       (set! log-format 'apache)]
     #:args ([site #f])
     site))

  (define site-config-path
    (simplify-path (path->complete-path (resolve-site-spec/cli remaining))))

  ;; Watching reloads site modules in-process, so site bytecode must be
  ;; cleared as the site loads (see camp/private/rerequire)
  (when watch?
    (live-reload? #t))

  (define current-site #f)

  (define (reload-site!)
    (set! current-site (load-site site-config-path)))

  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-error "loading site" (exn-message e))
                     (exit 1))])
    (reload-site!))

  (define (get-output-dir)
    (build-path (site-root current-site) (site-output-folder current-site)))

  (define (get-static-dir)
    (build-path (site-root current-site) (site-static-folder current-site)))

  (define manifest-path
    (if watch?
        (make-temporary-file "camp-static-~a")
        #f))

  (define (do-rebuild!)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-info (~a "  " (red "✗") " Build error:\n" (exn->string e)))
                       #f)])
      (define info (collect current-site))
      (build! current-site info)
      #t))

  (define (do-static-sync!)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-info (~a "  " (red "✗") " Static sync error:\n" (exn->string e)))
                       #f)])
      (sync-static-files (get-static-dir) (get-output-dir) manifest-path)
      #t))

  (define (rerequire-render-modules!)
    (for ([coll (in-list (site-collections current-site))])
      (define spec (collection-render-with coll))
      (when spec
        (with-handlers ([exn:fail? void])
          (rerequire! (car spec)))))
    (for ([feed (in-list (site-feeds current-site))])
      (with-handlers ([exn:fail? void])
        (rerequire! (car (feed-config-render-with feed)))))
    (when (site-default-render current-site)
      (with-handlers ([exn:fail? void])
        (rerequire! (car (site-default-render current-site))))))

  (unless (directory-exists? (get-output-dir))
    (log-camp-info (~a "  " (dim "Output folder not found, building...")))
    (unless (do-rebuild!)
      (exit 1))
    (log-camp-info (~a "  " (green "✓") " Initial build complete")))

  (define shutdown-server
    (start-server (get-output-dir) #:port port #:watch? watch? #:log-format log-format))

  (define (current-time-str)
    (define now (seconds->date (current-seconds)))
    (define (zpad n) (if (< n 10) (~a "0" n) (~a n)))
    (~a (zpad (date-hour now)) ":" (zpad (date-minute now)) ":" (zpad (date-second now))))

  (define (handle-change changed-path)
    (define rel-path (make-relative-path changed-path))
    (define change-type (path-change-type changed-path current-site site-config-path))

    (log-camp-info (~a "  " (dim (current-time-str)) "  " (cyan "●") " " rel-path " " (dim "changed")))

    (define-values (result rebuild-ms)
      (with-timing
        (case change-type
          [(config)
           (log-camp-info (~a "  " (dim "Reloading site config...")))
           (with-handlers ([exn:fail?
                            (λ (e)
                              (log-camp-info (~a "  " (red "✗") " Config error:\n" (exn->string e)))
                              #f)])
             (reload-site!)
             (do-rebuild!))]

          [(static)
           (log-camp-info (~a "  " (dim "Syncing static files...")))
           (do-static-sync!)]

          [(rkt)
           (log-camp-info (~a "  " (dim "Reloading modules...")))
           (rerequire-render-modules!)
           (log-camp-info (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)]

          [(source)
           (log-camp-info (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)]

          [else
           (log-camp-info (~a "  " (dim "Rebuilding...")))
           (do-rebuild!)])))

    (when result
      (log-camp-info (~a "  " (green "✓") " Done " (dim (format-duration rebuild-ms))))))

  (define (make-relative-path p)
    (define root (site-root current-site))
    (define rel (find-relative-path root p))
    (if (equal? rel p)
        (path->string (file-name-from-path p))
        (path->string rel)))

  ;; The watch paths are computed per watcher iteration, so a config reload
  ;; takes effect without restarting the watcher (handle-change runs on its
  ;; thread and cannot stop it)
  (define stop-watcher
    (if watch?
        (start-watcher! (λ () (get-watch-paths current-site site-config-path)) handle-change)
        void))

  (define (cleanup!)
    (log-camp-info (~a "  " (dim "Shutting down...")))
    (stop-watcher)
    (shutdown-server)
    (when (and manifest-path (file-exists? manifest-path))
      (delete-file manifest-path))
    (log-camp-info (~a "  " (green "✓") " Server stopped")))

  (with-handlers ([exn:break? (λ (e) (cleanup!))])
    (sync never-evt)))

;; ---------------------------------------------------------------------------
;; Deploy command

(define (run-deploy)
  (define remaining
    (command-line
     #:program "raco camp deploy"
     #:argv (current-command-line-arguments)
     #:args ([site #f])
     site))

  (define resolved-path (resolve-site-spec/cli remaining))

  (define site
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-error (~a "Error loading site: " (exn-message e)))
                       (exit 1))])
      (load-site resolved-path)))

  (define deploy-script (site-deploy-script site))

  (unless deploy-script
    (log-camp-error "No deploy-script configured in site configuration.")
    (log-camp-error "Add deploy-script = \"./deploy.sh\" to your site.rkt")
    (exit 1))

  (define root (site-root site))
  (define script-path (simplify-path (build-path root deploy-script)))

  (unless (file-exists? script-path)
    (log-camp-error (~a "Deploy script not found: " script-path))
    (exit 1))

  (define output-dir
    (path->string (simplify-path (build-path root (site-output-folder site)))))

  (log-camp-info (bold "camp deploy"))
  (log-camp-info (~a "  " (dim "Running") " " deploy-script " " output-dir))

  (define exit-code
    (parameterize ([current-directory root])
      (apply system*/exit-code script-path (list output-dir))))

  (if (zero? exit-code)
      (log-camp-info (~a "  " (green "✓") " " (bold "Deploy complete")))
      (log-camp-info (~a "  " (red "✗") " " (bold "Deploy failed") " (exit code " exit-code ")")))

  (exit exit-code))

;; ---------------------------------------------------------------------------
;; Print command

(define (run-print)
  (define open? #f)

  (define remaining
    (command-line
     #:program "raco camp print"
     #:argv (current-command-line-arguments)
     #:once-each
     [("--open") "Open PDF after building"
                 (set! open? #t)]
     #:args book-paths
     book-paths))

  (define books-to-build
    (if (null? remaining)
        (discover-books)
        (map (λ (p) (simplify-path (path->complete-path p))) remaining)))

  (when (null? books-to-build)
    (log-camp-error "No .book.rkt files found.")
    (exit 1))

  (for ([book-path (in-list books-to-build)])
    (unless (file-exists? book-path)
      (log-camp-error (~a "Book file not found: " book-path))
      (exit 1))

    (log-camp-info (~a (bold "camp print") " " (dim (path->string (file-name-from-path book-path)))))

    (define total-start (current-inexact-monotonic-milliseconds))
    (define pdf-path
      (with-handlers ([exn:fail?
                       (λ (e)
                         (log-error "building book" e)
                         (exit 1))])
        (build-book! (load-book book-path))))

    (define total-ms (- (current-inexact-monotonic-milliseconds) total-start))
    (cond
      [pdf-path
       (log-camp-info (~a "  " (green "✓") " " (bold (~a "Done in " (format-duration total-ms)))))
       (when open?
         (system* (find-executable-path "open") (path->string pdf-path)))]
      [else
       (log-camp-info (~a "  " (red "✗") " " (bold "Build failed")))
       (exit 1)])))

(define (discover-books)
  (define site-path (resolve-site-spec/cli #f))
  (define site
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-error (~a "Error loading site: " (exn-message e)))
                       (exit 1))])
      (load-site site-path)))
  (define root (site-root site))
  (for/list ([p (in-list (directory-list root #:build? #t))]
             #:when (regexp-match? #px"\\.book\\.rkt$" (path->string p)))
    p))

;; ---------------------------------------------------------------------------
;; New command

(define (run-new)
  (define name
    (command-line
     #:program "raco camp new"
     #:argv (current-command-line-arguments)
     #:args (name)
     name))

  ;; Validate name is a valid Racket identifier (roughly)
  (unless (regexp-match? #rx"^[a-z][a-z0-9-]*$" name)
    (log-camp-error (~a "Invalid site name '" name "'"))
    (log-camp-error "Name must start with a letter and contain only lowercase letters, numbers, and hyphens.")
    (exit 1))

  (define target-dir (build-path (current-directory) name))

  ;; Check if directory already exists
  (when (or (directory-exists? target-dir)
            (file-exists? target-dir))
    (log-camp-error (~a "'" name "' already exists."))
    (log-camp-error "Choose a different name or remove the existing directory.")
    (exit 1))

  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-camp-error (~a (red "✗") " Error:\n" (exn->string e)))
                     (exit 1))])
    (create-new-site target-dir name))

  (log-camp-info (~a "  " (green "✓") " Created " (bold name)))
  (log-camp-info (~a "  " (dim "Next steps:")))
  (log-camp-info (~a "    cd " name))
  (log-camp-info "    raco pkg install")
  (log-camp-info "    raco camp build")
  (log-camp-info "    raco camp serve"))

;; ---------------------------------------------------------------------------
;; Draft command

(define (run-draft)
  (define-values (collection-arg site-arg)
    (command-line
     #:program "raco camp draft"
     #:argv (current-command-line-arguments)
     #:args ([collection #f] [site #f])
     (values collection site)))

  ;; Determine if first arg is collection or site
  (define-values (coll-name site-spec)
    (cond
      [(and collection-arg site-arg)
       (values collection-arg site-arg)]
      [(and collection-arg (or (file-exists? collection-arg)
                               (resolve-site-spec collection-arg)))
       (values #f collection-arg)]
      [else
       (values collection-arg #f)]))

  (define resolved-path (resolve-site-spec/cli site-spec))

  (define site
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-error (~a "Error loading site: " (exn-message e)))
                       (exit 1))])
      (load-site resolved-path)))

  (define collections (site-collections site))
  (when (null? collections)
    (log-camp-error "Site has no collections defined.")
    (exit 1))

  (define coll
    (if coll-name
        (or (findf (λ (c) (equal? (collection-name c) coll-name)) collections)
            (begin
              (log-camp-error (~a "Collection not found: " coll-name))
              (log-camp-error (~a "Available: " (string-join (map collection-name collections) ", ")))
              (exit 1)))
        (car collections)))
  
  (displayln (~a "  " (dim "Collection:") " " (collection-name coll)))
  (display "  Title: ")
  
  (define title (read-line (current-input-port) 'any))

  (when (or (eof-object? title) (string=? (string-trim title) ""))
    (log-camp-error "Title cannot be empty.")
    (exit 1))

  (define file-path
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-camp-error (~a "Error creating draft: " (exn-message e)))
                       (exit 1))])
      (create-draft site (string-trim title) #:collection (collection-name coll))))

  (define rel-path
    (find-relative-path (site-root site) file-path))

  (log-camp-info (~a "  " (green "✓") " Created " (bold (path->string rel-path))))

  (define editor (getenv "EDITOR"))
  (when editor
    (log-camp-info (~a "  " (dim "Opening in") " " editor))
    (system* (find-executable-path editor) (path->string file-path))))

;; ---------------------------------------------------------------------------
;; Output Helpers

(define (log-phase-line phase-name desc timing-ms)
  (define bullet (cyan "●"))
  (define phase (~a phase-name #:min-width 10))
  (define description (dim desc))
  (define timing (if timing-ms (dim (format-duration timing-ms)) ""))

  (define prefix (~a "  " bullet " " phase description))
  (define prefix-len (+ 2 1 1 10 (string-length desc)))
  (define padding (max 1 (- LINE-WIDTH prefix-len (string-length (format-duration (or timing-ms 0))))))

  (log-camp-info (~a "  " bullet " " phase description
                     (make-string padding #\space)
                     timing)))

(define (log-error context e)
  (log-camp-error (~a "\n  " (red "✗") " Error " context ":\n" (exn->string e))))

(define (format-count-desc count1 noun1 count2 noun2)
  (~a (pluralize count1 noun1) " in " (pluralize count2 noun2)))

(define (pluralize n noun)
  (~a n " " noun (if (= n 1) "" "s")))

(main)
