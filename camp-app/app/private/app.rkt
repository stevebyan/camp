#lang racket/base

;; Camp App - Main application window and logic

(require camp/build
         (only-in camp/private/build sync-static-files output-path->url)
         (only-in camp/private/xref normalize-slug)
         (only-in camp/private/watch start-watcher! get-watch-paths path-change-type)
         (only-in camp/private/output format-duration with-timing)
         (only-in camp/private/dates current-site-timezone meta->moment)
         net/sendurl
         racket/exn
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/lazy-require
         racket/list
         racket/match
         racket/math
         racket/path
         (only-in camp/private/rerequire rerequire! live-reload?)
         racket/runtime-path
         racket/string
         racket/vector
         setup/dirs
         setup/getinfo
         camp/app/private/settings
         camp/app/private/gui
         camp/app/private/app-appearance
         (only-in camp/app/private/editor-utils use-internal-editor? font-slot-label)
         (only-in camp/app/private/theme import-theme-file scheme-name scheme-dark?)
         camp/app/private/site-utils
         camp/app/private/subprocess-env)

(provide run-app)

;; The main window needs neither the web server nor framework, which the
;; editor, its fonts and its color schemes build on; together they take as
;; long to load as everything else, so they load only once the window is up
(lazy-require [camp/serve (start-server)]
              [camp/app/private/appearance (init-appearance!)]
              [camp/app/private/fonts (apply-active-font-slot! slot-font)]
              [camp/app/private/editor (open-editor! close-all-editors!)])

;; The app reloads site modules in-process for its lifetime, so site
;; bytecode must be cleared as sites load (see camp/private/rerequire)
(live-reload? #t)

;; ============================================================================
;; Helper functions

(define (vec-ref? v idx)
  (cond
    [(not (vector? v)) #f]
    [(zero? (vector-length v)) #f]
    [else (vector-ref v idx)]))

;; ============================================================================
;; Toolbar icons

(define-runtime-path icon-new-page "icons/new-page.png")
(define-runtime-path icon-build    "icons/build.png")
(define-runtime-path icon-start    "icons/start.png")
(define-runtime-path icon-stop     "icons/stop.png")
(define-runtime-path icon-publish  "icons/publish.png")
(define-runtime-path icon-app      "icons/app.png")
(define-runtime-path pkg-dir      "../..")

(define icon-size 32)
(define about-icon-size 128)

(define (scale-bitmap path size)
  (and (file-exists? path)
       (let* ([src (read-bitmap path)]
              [w (send src get-width)]
              [h (send src get-height)]
              [bs (get-display-backing-scale)]
              [dest (make-bitmap size size #:backing-scale bs)]
              [dc (send dest make-dc)])
         (send dc set-smoothing 'smoothed)
         (send dc draw-bitmap-section-smooth src 0 0 size size 0 0 w h)
         dest)))

(define bmp-new-page (scale-bitmap icon-new-page icon-size))
(define bmp-build    (scale-bitmap icon-build icon-size))
(define bmp-start    (scale-bitmap icon-start icon-size))
(define bmp-stop     (scale-bitmap icon-stop icon-size))
(define bmp-publish  (scale-bitmap icon-publish icon-size))
(define bmp-app      (scale-bitmap icon-app about-icon-size))

(define app-version
  (let ([info (get-info/full pkg-dir)])
    (if info (info 'version (λ () "??")) "??")))

;; ============================================================================
;; Core observables
;;
;; The site observable is the foundation - everything else derives from it.

(define (load-site-from-spec spec)
  (and spec
       (with-handlers ([exn:fail?
                        (λ (e)
                          (render (?dialog (format "Error loading site: ~a" (exn-message e))))
                          (remove-from-pref! @sites spec)
                          #f)])
         (define path (resolve-site-spec spec))
         (cond
           [(not path)
            (render (?dialog (format "Cannot resolve site: ~a" spec)))
            (remove-from-pref! @sites spec)
            #f]
           [else (load-site path)]))))

(define/obs @site #f)
;; Renotifying the current spec (to reset the site menu) doesn't reload it
(obs-observe! @site-selection
              (let ([loaded (obs-peek @site-selection)])
                (λ (spec)
                  (unless (equal? spec loaded)
                    (set! loaded spec)
                    (@site . := . (load-site-from-spec spec))))))

(define @site-root (obs-map @site (λ (s) (and s (site-root s)))))

(define @output-folder
  (obs-map @site
           (λ (s) (and s (build-path (site-root s) (site-output-folder s))))))

(define @has-site? (obs-map @site (λ (s) (and s #t))))

;; ============================================================================
;; Build

;; Builds are serialized: the startup build, watcher rebuilds, and the Build
;; button may otherwise interleave doc loads and output writes.
(define build-sema (make-semaphore 1))

(define (build-site!)
  (define site (obs-peek @site))
  (when site
    (call-with-semaphore
     build-sema
     (λ ()
       (log-msg "Building site...")
       (with-handlers ([exn:fail?
                        (λ (e)
                          (log-msg "Build error:\n~a" (exn->string e)))])
         (define start-time (current-inexact-milliseconds))
         (define info (collect site))
         (build! site info)
         (define elapsed (- (current-inexact-milliseconds) start-time))
         (log-msg "Build complete (~ams)" (inexact->exact (round elapsed))))))))

(define (build-site-async!)
  (void (thread build-site!)))

;; The selected site loads after the main window appears; until its pages
;; are listed, the controls that would switch sites stay disabled
(define/obs @loading? (and (obs-peek @site-selection) #t))

(define (load-selected-site!)
  (define spec (obs-peek @site-selection))
  (when spec
    (log-msg "Loading ~a…" (site-spec->display-name spec))
    (dynamic-wind
     void
     (λ () (@site . := . (load-site-from-spec spec)))
     (λ () (@loading? . := . #f)))
    (build-site!)))

(define @switching-enabled? (obs-map @loading? not))

;; For watcher-triggered config reloads: unlike load-site-from-spec, a config
;; error (e.g. a half-saved edit) keeps the previous site instead of removing
;; it from the app. Updating @site cascades into a rebuild via @folders.
(define (reload-site!)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-msg "Site config error (keeping previous config):\n~a" (exn->string e))
                     #f)])
    (@site . := . (load-site (resolve-site-spec (obs-peek @site-selection))))
    #t))

;; ============================================================================
;; Refresh triggers and derived data

(define/obs @refresh-counter 0)

(define (trigger-refresh!)
  (obs-update! @refresh-counter add1)
  (build-site-async!))

(define @folders
  (obs-combine
   (λ (site _counter)
     (if site (get-source-folders site) (vector)))
   @site
   @refresh-counter))

(define @folder-selection (@ (vec-ref? (obs-peek @folders) 0)))
;; The folder table's highlight; see track-folder-selection!
(define/obs @folder-index #f)

(define (folder-index folders folder)
  (for/first ([f (in-vector folders)] [i (in-naturals)] #:when (equal? f folder)) i))

;; Keeps a folder selected whenever the folder list changes (a site switch
;; picks the new site's first folder) and re-shows it in the table, which
;; drops its highlight whenever its entries change. Installed after the main
;; window renders, so the table's own update is queued first.
(define (track-folder-selection!)
  (obs-observe!
   @folders
   (λ (folders)
     (unless (folder-index folders (obs-peek @folder-selection))
       (@folder-selection . := . (vec-ref? folders 0)))
     (@folder-index . := . (folder-index folders (obs-peek @folder-selection))))))

(define @source-doc-selection (@ ""))
(define-values (date-col title-col status-col file-col fullpath) (values 0 1 2 3 4))
(define @source-sorting (@ (cons date-col string-ci>?)))

(define month-names #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define (format-display-date date-val)
  (define date-str (~a date-val))
  (cond
    [(regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})" date-str)
     => (λ (m)
          (define month (string->number (list-ref m 2)))
          (define day (string->number (list-ref m 3)))
          (format "~a ~a, ~a" (vector-ref month-names (sub1 month)) day (list-ref m 1)))]
    [else date-str]))

(define (source-entry->row entry)
  (vector (format-display-date (vector-ref entry date-col))
          (~a (vector-ref entry title-col))
          (vector-ref entry status-col)
          (~a (vector-ref entry file-col))))

(define (sort-pages pages sorting)
  (define sort-key (car sorting))
  (vector-sort pages (λ (a b) ((cdr sorting) (vector-ref a sort-key) (vector-ref b sort-key)))))

(define @sources-in-folder
  (obs-combine
   (λ (site folder sorting _counter)
     (if (and site folder)
         (sort-pages (get-pages-in-folder site folder) sorting)
         (vector)))
   @site
   @folder-selection
   @source-sorting
   @refresh-counter))

(define/obs @stop-server-proc #f)


;; ============================================================================
;; Server management

(define localhost-port 8000)

(define (mindful-demure-server-stop)
  (match (obs-peek @stop-server-proc)
    [(? procedure? stop)
     (log-msg "Stopping preview server")
     (stop)
     (@stop-server-proc . := . #f)
     #t]
    [_ #f]))

(define (rerequire-render-modules! site)
  (for ([coll (in-list (site-collections site))])
    (define spec (collection-render-with coll))
    (when spec
      (with-handlers ([exn:fail? void])
        (rerequire! (car spec)))))
  (for ([feed (in-list (site-feeds site))])
    (with-handlers ([exn:fail? void])
      (rerequire! (car (feed-config-render-with feed)))))
  (when (site-default-render site)
    (with-handlers ([exn:fail? void])
      (rerequire! (car (site-default-render site))))))

;; ============================================================================
;; Components: Site selection

;; Editor windows belong to the site they were opened from (a save rebuilds
;; the current site), so they close first; cancelling a save prompt cancels
;; the switch
(define (on-site-select v)
  (cond
    [(close-editors!)
     (log-msg "Switched to site: ~a" v)
     (mindful-demure-server-stop)
     (@site-selection . := . v)
     (build-site-async!)]
    [else (@site-selection . <~ . values)]))

(define :sites-choice
  (choice @sites on-site-select
          #:selection @site-selection
          #:choice->label site-spec->display-name
          #:label "Sites"
          #:enabled? @switching-enabled?
          #:stretch '(#t #f)))

;; ============================================================================
;; Components: Folder table

(define (src-folder->friendly p)
  (define name (path->string (last (explode-path p))))
  (string-append "📂 " (string-titlecase name)))

(define (path->table-row p) (vector (src-folder->friendly p)))
(define (get-folder idx) (vec-ref? (obs-peek @folders) idx))

(define (on-folder-select evt _entries idx)
  (when idx
    (case evt
      [(select) (@folder-selection . := . (get-folder idx))]
      [(dclick) (render (?dialog (path->string (get-folder idx))))])))

(define :folders-table
  (table '("Folders") @folders
         on-folder-select
         #:selection @folder-index
         #:entry->row path->table-row))

;; ============================================================================
;; Components: Source documents table

;; A save in the built-in editor rebuilds directly, except while the preview
;; server runs — its file watcher already rebuilds, so only refresh the tables
(define (handle-editor-save! _p)
  (if (obs-peek @stop-server-proc)
      (obs-update! @refresh-counter add1)
      (trigger-refresh!)))

;; Set once the editor module loads; until then there are no editor windows
;; to close, and closing none shouldn't load it
(define editor-used? #f)

(define (close-editors!)
  (or (not editor-used?) (close-all-editors!)))

(define (edit-source p)
  (cond
    [(use-internal-editor? (obs-peek @editor))
     (force editor-support)
     (set! editor-used? #t)
     (open-editor! p #:on-save handle-editor-save!)]
    [else (edit-source/external p)]))

(define (edit-source/external p)
  (thread
   (λ ()
     (define editor-path (obs-peek @editor))
     (define file-arg (path->string p))
     (define-values (sp out in err)
       (cond
         [(and (non-empty-string? editor-path)
               (eq? (system-type 'os) 'macosx)
               (regexp-match? #rx"\\.app$" editor-path))
          (subprocess #f #f #f "/usr/bin/open" "-a" editor-path file-arg)]
         [(non-empty-string? editor-path)
          (subprocess #f #f #f editor-path file-arg)]
         [(eq? (system-type 'os) 'macosx)
          (subprocess #f #f #f "/usr/bin/open" file-arg)]
         [else
          (subprocess #f #f #f (find-executable-path "xdg-open") file-arg)]))
     (subprocess-wait sp)
     (log-msg "Opened: ~a" (file-name-from-path p))
     (close-input-port out)
     (close-output-port in)
     (close-input-port err))))

(define (get-source idx)
  (vector-ref (vec-ref? (obs-peek @sources-in-folder) idx) fullpath))

(define (get-source-url idx)
  (vector-ref (vec-ref? (obs-peek @sources-in-folder) idx) file-col))

(define (delete-source p)
  (define name (path->string (file-name-from-path p)))
  (define confirm
    (message-box "Delete page"
                 (format "Delete ~a?" name)
                 (get-main-frame) '(yes-no caution)))
  (when (eq? confirm 'yes)
    (delete-file p)
    (log-msg "Deleted: ~a" name)
    (trigger-refresh!)))

(define (update-sorting col)
  (define cur-sort (obs-peek @source-sorting))
  (cond
    [(= col (car cur-sort))
     (@source-sorting . := . (cons col (if (eq? (cdr cur-sort) string-ci>?)
                                            string-ci<?
                                            string-ci>?)))]
    [else
     (@source-sorting . := . (cons col (cdr cur-sort)))]))

(define (show-source-context-menu list-box x y)
  (define idx (send list-box get-selection))
  (when idx
    (define p (get-source idx))
    (define page-url (get-source-url idx))
    (define menu (new popup-menu%))
    (new menu-item% [parent menu] [label "Edit"]
         [callback (λ (_item _evt) (edit-source p))])
    (new menu-item% [parent menu] [label "Preview"]
         [callback (λ (_item _evt)
                     (when (ensure-server-running!)
                       (send-url (format "http://localhost:~a~a" localhost-port page-url))))])
    (new separator-menu-item% [parent menu])
    (new menu-item% [parent menu] [label "Delete"]
         [callback (λ (_item _evt) (delete-source p))])
    (send list-box popup-menu menu x y)))

(define (on-source-select evt _entries idx)
  (when idx
    (case evt
      [(column) (update-sorting idx)]
      [(select) (@source-doc-selection . := . (get-source idx))]
      [(dclick) (edit-source (get-source idx))])))

(define source-table-mixin
  (λ (%)
    (class %
      (super-new)
      (define/override (on-subwindow-event receiver event)
        (cond
          [(and (eq? receiver this)
                (send event button-down? 'right))
           (show-source-context-menu this
                                     (send event get-x)
                                     (send event get-y))]
          [else (super on-subwindow-event receiver event)])))))

(define :source-docs-table
  (table '("Date" "Title" "Status" "URL") @sources-in-folder
         on-source-select
         #:entry->row source-entry->row
         #:mixin source-table-mixin
         #:column-widths '((0 120 80 150)
                           (1 300 100 600)
                           (2 60 50 100)
                           (3 200 100 300))))

;; ============================================================================
;; Components: Toolbar

(define button-size '(120 50))

(define (toolbar-button label action #:icon [icon #f] #:enabled? [@enabled? #t])
  (button (if icon (list icon label 'top) label)
          action
          #:enabled? @enabled?
          #:min-size button-size
          #:style '(multi-line)))

(define (on-new-page-click)
  (render (?new-page)))

(define (ensure-server-running!)
  (cond
    [(obs-peek @stop-server-proc) #t]
    [else
     (define site (obs-peek @site))
     (define server-output (obs-peek @output-folder))
     (cond
       [(not (and site server-output)) #f]
       [else
        (log-msg "Starting preview server (port ~a)" localhost-port)
        (define shutdown-server (start-server server-output #:port localhost-port #:watch? #t))

        ;; File watching; the site is peeked per event (and per watcher
        ;; iteration, for watch paths) so config reloads take effect live
        (define site-config-path
          (simplify-path (path->complete-path (resolve-site-spec (obs-peek @site-selection)))))
        (define manifest-path (make-temporary-file "camp-static-~a"))

        (define (handle-change changed-path)
          (define site (obs-peek @site))
          (when site
            (define output (obs-peek @output-folder))
            (define rel (find-relative-path (site-root site) changed-path))
            (define rel-str (if (equal? rel changed-path)
                                (path->string (file-name-from-path changed-path))
                                (path->string rel)))
            (define change-type (path-change-type changed-path site site-config-path))
            (log-msg "~a changed" rel-str)
            (define-values (result rebuild-ms)
              (with-timing
                (case change-type
                  [(config)
                   (log-msg "Reloading site config...")
                   (define ok? (reload-site!))
                   (when ok?
                     (unless (equal? (obs-peek @output-folder) server-output)
                       (log-msg "Output folder changed; restart the preview server to serve it"))
                     (build-site!))
                   ok?]
                  [(static)
                   (define static-dir (build-path (site-root site) (site-static-folder site)))
                   (with-handlers ([exn:fail? (λ (e) (log-msg "Static sync error:\n~a" (exn->string e)) #f)])
                     (sync-static-files static-dir output manifest-path)
                     #t)]
                  [(rkt)
                   (rerequire-render-modules! site)
                   (build-site!)
                   #t]
                  [else (build-site!) #t])))
            (when result
              (log-msg "Done (~a)" (format-duration rebuild-ms)))))

        (define (watch-paths)
          (define s (obs-peek @site))
          (if s (get-watch-paths s site-config-path) '()))
        (define stop-watcher (start-watcher! watch-paths handle-change))

        (@stop-server-proc . := .
         (λ ()
           (stop-watcher)
           (shutdown-server)
           (when (file-exists? manifest-path)
             (delete-file manifest-path))))
        #t])]))

(define (on-start-preview-click)
  (match (obs-peek @stop-server-proc)
    [#f (when (ensure-server-running!)
          (send-url (format "http://localhost:~a" localhost-port)))]
    [_ (mindful-demure-server-stop)]))

(define (on-publish-click)
  (define site (obs-peek @site))
  (when site
    (define deploy-script (site-deploy-script site))
    (cond
      [(not deploy-script)
       (render (?dialog "No deploy script configured in site.rkt"))]
      [else
       (log-msg "Running deploy script: ~a" deploy-script)
       (thread
        (λ ()
          (define root (site-root site))
          (define output-folder (path->string (obs-peek @output-folder)))
          (define script-path (build-path root deploy-script))
          (define-values (sp out in err)
            (parameterize ([current-directory root]
                           [current-environment-variables (racket-subprocess-env)])
              (subprocess #f #f 'stdout (path->string script-path) output-folder)))
          (close-output-port in)
          (for ([line (in-lines out)])
            (log-msg "  ~a" line))
          (close-input-port out)
          (subprocess-wait sp)
          (define exit-code (subprocess-status sp))
          (if (zero? exit-code)
              (log-msg "Deploy complete")
              (log-msg "Deploy failed (exit code ~a)" exit-code))))])))

(define cmd-held? (box #f))

(define build-button-mixin
  (λ (%)
    (class %
      (super-new)
      (define/override (on-subwindow-event receiver event)
        (when (is-a? event mouse-event%)
          (set-box! cmd-held? (send event get-meta-down)))
        (super on-subwindow-event receiver event)))))

(define (full-rebuild!)
  (define site (obs-peek @site))
  (when site
    (define site-arg
      (let ([spec (obs-peek @site-selection)])
        (cond [(symbol? spec) (symbol->string spec)]
              [(path? spec) (path->string spec)]
              [else (~a spec)])))
    (log-msg "Full rebuild (subprocess)...")
    (define-values (sp out in err)
      (parameterize ([current-environment-variables (racket-subprocess-env)])
        (subprocess #f #f #f
                    (path->string (build-path (find-console-bin-dir) "raco"))
                    "camp" "build" "--verbose" site-arg)))
    (close-output-port in)
    (define err-thread
      (thread (λ ()
                (for ([line (in-lines err)]) (log-msg "  ~a" line))
                (close-input-port err))))
    (for ([line (in-lines out)]) (log-msg "  ~a" line))
    (close-input-port out)
    (thread-wait err-thread)
    (subprocess-wait sp)
    (define exit-code (subprocess-status sp))
    (if (zero? exit-code)
        (log-msg "Full rebuild complete")
        (log-msg "Full rebuild failed (exit code ~a)" exit-code))))

(define (on-build-click)
  (if (unbox cmd-held?)
      (thread full-rebuild!)
      (thread build-site!)))

(define :startstop-button
  (button (if bmp-start (list bmp-start "Start preview" 'top) "Start preview")
          on-start-preview-click
          #:enabled? @has-site?
          #:min-size button-size
          #:style '(multi-line)
          #:mixin (λ (%)
                    (class %
                      (super-new)
                      (obs-observe!
                       @stop-server-proc
                       (λ (v)
                         (queue-callback
                          (λ ()
                            (send this set-label (if v "Stop preview" "Start preview"))
                            (define bmp (if v bmp-stop bmp-start))
                            (when bmp (send this set-label bmp))))))))))

(define :toolbar
  (hpanel
   #:stretch '(#t #f)
   (toolbar-button "New page" on-new-page-click #:icon bmp-new-page #:enabled? @has-site?)
   (button (if bmp-build (list bmp-build "Build site" 'top) "Build site")
           on-build-click
           #:enabled? @has-site?
           #:min-size button-size
           #:style '(multi-line)
           #:mixin build-button-mixin)
   :startstop-button
   (button (if bmp-publish (list bmp-publish "Publish" 'top) "Publish")
          on-publish-click
          #:enabled? (obs-map @site (λ (s) (and s (site-deploy-script s) #t)))
          #:min-size button-size
          #:style '(multi-line))))

;; ============================================================================
;; Components: Menu

(define (on-add-site)
  (render (?add-site)))

(define (on-remove-site)
  (define current (obs-peek @site-selection))
  (when current
    (define name (site-spec->display-name current))
    (define confirm
      (message-box "Remove site"
                   (format "Remove ~a from the site list?" name)
                   (get-main-frame) '(yes-no caution)))
    (when (eq? confirm 'yes)
      (remove-from-pref! @sites current)
      (@site-selection . := . (match (obs-peek @sites)
                                ['() #f]
                                [(list* first _) first])))))

(define :main-menu
  (menu-bar
   (menu
    "File"
    (menu-item "New page" on-new-page-click #:shortcut '(cmd #\N) #:enabled? @has-site?)
    (menu-item-separator)
    (menu-item "Build" (λ () (thread build-site!)) #:shortcut '(cmd #\B) #:enabled? @has-site?)
    (menu-item "Full Rebuild" (λ () (thread full-rebuild!)) #:shortcut '(cmd shift #\B) #:enabled? @has-site?)
    (menu-item-separator)
    (menu-item "Add site…" on-add-site #:enabled? @switching-enabled?)
    (menu-item "Remove this site…" on-remove-site #:enabled? @switching-enabled?)
    (menu-item-separator)
    (menu-item "Preferences…" (λ () (render (?prefs)))))))

;; ============================================================================
;; Dialogs

(define about-url "https://joeldueck.com/what-about/camp")

(define link-font (make-object font% 13 'default 'normal 'normal #t))

(define (?about)
  (define-values (close! closing-mixin) (make-mix-close))
  (dialog
   #:title "About Camp Computer"
   #:mixin closing-mixin
   #:size '(300 #f)
   #:alignment '(center center)
   (vpanel
    #:alignment '(center center)
    (image bmp-app)
    (text "Camp Computer")
    (text (format "Version ~a" app-version))
    (spacer)
    (button about-url (λ () (send-url about-url))
            #:font link-font
            #:stretch '(#f #f))
    (spacer)
    (button "Close" close!))))

(define (?new-page)
  (define-values (close! closing-mixin) (make-mix-close))
  (define/obs @title "")
  (define/obs @date (date->string (current-date) "~Y-~m-~d"))
  (define/obs @slug "")

  ;; Determine taxonomies, output-pattern meta keys, and output pattern
  (define site (obs-peek @site))
  (define folder (obs-peek @folder-selection))
  (define coll (and site folder (folder->collection site folder)))
  (define taxonomy-names (if coll (collection-taxonomies coll) '()))
  (define output-pattern (and coll (collection-output-paths coll)))
  (define path-meta-names (if output-pattern (pattern-meta-keys output-pattern) '()))

  ;; Unified meta names: taxonomies + path metas, deduplicated, preserving order
  (define all-meta-names
    (remove-duplicates (append taxonomy-names path-meta-names)))
  (define meta-obs
    (for/list ([name (in-list all-meta-names)])
      (cons name (@ ""))))

  (define (peek-metas-hash)
    (for/hasheq ([pair (in-list meta-obs)]
                 #:unless (string=? (obs-peek (cdr pair)) ""))
      (values (string->symbol (car pair)) (obs-peek (cdr pair)))))

  (define @url-preview
    (apply obs-combine
     (λ (title date slug . _meta-vals)
       (define actual-slug (normalize-slug (if (non-empty-string? slug) slug title)))
       (if (and output-pattern (non-empty-string? actual-slug))
           (with-handlers ([exn:fail? (λ (_) "")])
             (define date-val
               (and (non-empty-string? date)
                    (parameterize ([current-site-timezone (site-timezone site)])
                      (meta->moment date))))
             (define metas (peek-metas-hash))
             (output-path->url
              (format-output-path output-pattern actual-slug date-val
                                  (if (hash-empty? metas) #f metas))))
           ""))
     @title @date @slug
     (map cdr meta-obs)))

  (define (create-page)
    (when (and site folder)
      (define title (obs-peek @title))
      (define date (obs-peek @date))
      (define slug (obs-peek @slug))
      (define source-ext (site-sources site))
      (define filename (string-append (normalize-slug title) source-ext))
      (define filepath (build-path folder filename))

      (define meta-lines
        (for/list ([pair (in-list meta-obs)]
                   #:unless (string=? (obs-peek (cdr pair)) ""))
          (format "~a: ~a\n" (car pair) (obs-peek (cdr pair)))))

      (define pkg (site-racket-collection site))
      (define content
        (string-append
         (if pkg (string-append "#lang punct " pkg "\n") "#lang punct\n")
         "\n---\n"
         (format "title: ~a\n" title)
         (format "date: ~a\n" date)
         (if (non-empty-string? slug) (format "slug: ~a\n" slug) "")
         (apply string-append meta-lines)
         "---\n\n"
         "Write your content here.\n"))

      (display-to-file content filepath #:exists 'error)
      (log-msg "Created: ~a" filename)
      (trigger-refresh!)
      (edit-source filepath))
    (close!))

  (dialog
   #:title "New page"
   #:mixin closing-mixin
   #:size '(400 #f)
   (apply vpanel
     (append
       (list
         (input @title (λ (_action s) (@title . := . s)) #:label "Title")
         (input @date (λ (_action s) (@date . := . s)) #:label "Date")
         (input @slug (λ (_action s) (@slug . := . s)) #:label "Slug (optional)")
         (text @url-preview))
       (for/list ([pair (in-list meta-obs)])
         (input (cdr pair)
                (λ (_action s) ((cdr pair) . := . s))
                #:label (string-titlecase (car pair))))
       (list
         (hpanel
           (button "Create" create-page)
           (button "Cancel" close!)))))))

(define (?add-site)
  (define-values (close! closing-mixin) (make-mix-close))
  (define/obs @path "")

  (define (add-by-path)
    (define p (obs-peek @path))
    (unless (string=? p "")
      (cond
        [(file-exists? p)
         (add-to-pref! @sites p)
         (@site-selection . := . p)
         (close!)]
        [(directory-exists? p)
         (define site-file (build-path p "site.rkt"))
         (if (file-exists? site-file)
             (begin
               (add-to-pref! @sites (path->string site-file))
               (@site-selection . := . (path->string site-file))
               (close!))
             (render (?dialog "No site.rkt found in that folder")))]
        [else
         ;; Try as package name
         (define sym (string->symbol p))
         (define resolved (resolve-site-spec sym))
         (if resolved
             (begin
               (add-to-pref! @sites sym)
               (@site-selection . := . sym)
               (close!))
             (render (?dialog (format "Cannot find: ~a" p))))])))

  (define (browse-folder)
    (define path (get-directory "Select site folder" (get-main-frame)))
    (when path
      (@path . := . (path->string path))))

  (dialog
   #:title "Add site"
   #:mixin closing-mixin
   #:size '(500 #f)
   (vpanel
    (text "Enter a path to site.rkt, a folder containing site.rkt,")
    (text "or a package name:")
    (hpanel
     (input @path (λ (action s) (@path . := . s) (when (eq? action 'return) (add-by-path)))
            #:min-size '(350 #f))
     (button "Browse…" browse-folder))
    (hpanel
     (button "Add" add-by-path)
     (button "Cancel" close!)))))

(define (external-editor-label app)
  (if (non-empty-string? app)
      (format "External editor (~a)"
              (regexp-replace #rx"[.]app$" (path->string (file-name-from-path app)) ""))
      "External editor"))

(define (choose-editor!)
  (define default-dir
    (and (eq? (system-type 'os) 'macosx)
         (string->path "/Applications/")))
  (define path (get-file "Choose editor" (get-main-frame) default-dir))
  (when path
    (@external-editor . := . (path->string path))
    (@editor . := . (path->string path))))

(define (use-external-editor!)
  (define app (obs-peek @external-editor))
  (cond
    [(non-empty-string? app) (@editor . := . app)]
    [else (choose-editor!)
          ;; a cancelled chooser leaves the radio clicked; renotify to clear it
          (@editor . <~ . values)]))

;; HIG two-column form: right-aligned label column, left-aligned controls
;; #:top-inset pins the label to the top of a tall control, dropped far
;; enough to sit level with that control's first line.
(define ((make-pref-row label-width) label #:top-inset [inset #f] . controls)
  (hpanel
   #:stretch '(#t #f)
   #:spacing 6
   #:alignment (list 'left (if inset 'top 'center))
   (hpanel #:min-size (list label-width #f)
           #:margin (list 0 (or inset 0))
           #:stretch '(#f #f)
           #:alignment '(right center)
           (text label))
   (apply hpanel #:spacing 4 #:alignment '(left center) controls)))

;; Rows inside the group box give up its inset, so every colon lines up
(define pref-group-inset 8)
(define pref-row (make-pref-row (+ 94 pref-group-inset)))
(define group-pref-row (make-pref-row 94))

(define pref-caption-color (make-color 120 120 120))

(define (pref-caption s)
  (text s #:font small-control-font #:color pref-caption-color))

(define (pref-group-gap)
  (hpanel #:min-size '(#f 8) #:stretch '(#t #f)))

(define pref-wide-control 240)

(define (change-font-slot! idx)
  (force editor-support)
  (define f (choose-font (get-main-frame) (slot-font idx)))
  (when f
    (define face (or (send f get-face)
                     (get-family-builtin-face (send f get-family))))
    (define size (exact-round (send f get-size)))
    (@font-slots . <~ . (λ (slots) (list-set slots idx (list face size))))))

;; A radio-box% fixes its labels at creation and lays out only its own items.
;; Each option is therefore a one-item group: rebuilt when its label changes,
;; and free to share a row with other controls.
(define (solo-radio label @selected? select!)
  (observable-view
   (@ label)
   (λ (label)
     (radios '(#t)
             (λ (on?) (when on? (select!)))
             #:choice->label (λ (_) label)
             #:selection @selected?
             ;; radio-box% selects its first item unless told otherwise
             #:mixin (λ (%) (class % (super-new [selection #f])))))))

;; The stretchy panel pushes every row's button to the same right edge
(define (font-slot-row idx)
  (hpanel
   #:alignment '(left center)
   (hpanel
    #:min-size '(200 #f)
    #:alignment '(left center)
    (solo-radio (obs-map @font-slots (λ (slots) (font-slot-label idx (list-ref slots idx))))
                (obs-map @font-slot (λ (active) (= active idx)))
                (λ () (@font-slot . := . idx))))
   (button "Change…" (λ () (change-font-slot! idx)) #:margin '(2 0))))

(define (scheme-display-name v)
  (match v
    ['classic "DrRacket classic"]
    ['white-on-black "DrRacket white-on-black"]
    [_ v]))

(define (scheme-choices dark?)
  (obs-map @color-schemes
           (λ (schemes)
             (cons (if dark? 'white-on-black 'classic)
                   (for/list ([s (in-list schemes)]
                              #:when (eq? dark? (scheme-dark? s)))
                     (scheme-name s))))))

(define (import-scheme!)
  (define p (get-file "Import theme" (get-main-frame) #f #f #f null
                      '(("Theme files" "*.tmTheme;*.json"))))
  (when p
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-msg "Theme import failed: ~a" (exn-message e))
                       (message-box "Import Failed" (exn-message e)
                                    (get-main-frame) '(ok stop)))])
      (define s (import-theme-file p))
      (@color-schemes . <~ .
       (λ (schemes)
         (if (findf (λ (x) (equal? (scheme-name x) (scheme-name s))) schemes)
             (for/list ([x (in-list schemes)])
               (if (equal? (scheme-name x) (scheme-name s)) s x))
             (append schemes (list s)))))
      (log-msg "Imported ~a theme: ~a"
               (if (scheme-dark? s) "dark" "light") (scheme-name s)))))

(define (remove-scheme! s)
  (when s
    (when (equal? (obs-peek @light-scheme) (scheme-name s))
      (@light-scheme . := . 'classic))
    (when (equal? (obs-peek @dark-scheme) (scheme-name s))
      (@dark-scheme . := . 'white-on-black))
    (@color-schemes . <~ . (λ (schemes) (remove s schemes)))))

(define (?prefs)
  (define-values (close! closing-mixin) (make-mix-close))
  (define @camp-editor? (obs-map @editor use-internal-editor?))
  (define/obs @scheme-selection #f)
  (dialog
   #:title "Preferences"
   #:mixin closing-mixin
   (vpanel
    #:margin '(20 16)
    #:spacing 2
    #:alignment '(left top)
    (pref-row "Appearance:"
              (choice '(light dark system)
                      (λ (m) (when m (@appearance-mode . := . m)))
                      #:selection @appearance-mode
                      #:choice->label (λ (m) (case m
                                               [(light) "Light"]
                                               [(dark) "Dark"]
                                               [else "Follow System"]))
                      #:min-size (list pref-wide-control #f)))
    (pref-group-gap)
    (pref-row "Editor:"
              (solo-radio "Camp Editor" @camp-editor? (λ () (@editor . := . ""))))
    (pref-row ""
              (solo-radio (obs-map @external-editor external-editor-label)
                          (obs-map @camp-editor? not)
                          use-external-editor!)
              (button "Choose Editor App…" choose-editor!))
    (pref-group-gap)
    (group
     "Camp Editor"
     #:stretch '(#t #f)
     (vpanel
      #:margin '(0 6)
      #:spacing 2
      #:alignment '(left top)
      (group-pref-row "Options:"
                      (checkbox (λ (on?) (@vim-mode . := . (and on? #t)))
                                #:label "Vim keybindings"
                                #:checked? @vim-mode))
      (group-pref-row ""
                      (checkbox (λ (on?) (@line-numbers? . := . (and on? #t)))
                                #:label "Show line numbers"
                                #:checked? @line-numbers?))
      (group-pref-row ""
                      (checkbox (λ (on?) (@auto-fill? . := . (and on? #t)))
                                #:label "Break lines at the wrap column while typing"
                                #:checked? @auto-fill?))
      (group-pref-row "Wrap column:"
                      (input (obs-map @fill-column number->string)
                             (λ (_action s)
                               (define n (string->number s))
                               (when (exact-positive-integer? n)
                                 (@fill-column . := . n)))
                             #:min-size '(56 #f)
                             #:stretch '(#f #f))
                      (pref-caption "⌘J re-wraps the current paragraph"))
      (pref-group-gap)
      (group-pref-row "Editor font:" #:top-inset 7
                      (vpanel
                       #:stretch '(#f #f)
                       #:alignment '(left top)
                       (font-slot-row 0)
                       (font-slot-row 1)
                       (font-slot-row 2)
                       (pref-caption "⇧⌘F switches to the next font")))
      (pref-group-gap)
      (group-pref-row "Light scheme:"
                      (choice (scheme-choices #f)
                              (λ (v) (when v (@light-scheme . := . v)))
                              #:selection @light-scheme
                              #:choice->label scheme-display-name
                              #:min-size (list pref-wide-control #f)))
      (group-pref-row "Dark scheme:"
                      (choice (scheme-choices #t)
                              (λ (v) (when v (@dark-scheme . := . v)))
                              #:selection @dark-scheme
                              #:choice->label scheme-display-name
                              #:min-size (list pref-wide-control #f)))
      (group-pref-row "Themes:" #:top-inset 4
                      (vpanel
                       #:stretch '(#f #f)
                       #:alignment '(left top)
                       (table '("Name" "Mode")
                              (obs-map @color-schemes list->vector)
                              (λ (_evt entries idx)
                                (@scheme-selection . := . (and idx (vector-ref entries idx))))
                              #:entry->row (λ (s) (vector (scheme-name s)
                                                          (if (scheme-dark? s) "Dark" "Light")))
                              #:column-widths '((0 150) (1 66))
                              #:margin '(2 2)
                              #:min-size (list pref-wide-control 106)
                              #:stretch '(#f #f))
                       (hpanel
                        (button "Import Theme…" (λ () (import-scheme!)))
                        (button "Remove"
                                (λ ()
                                  (remove-scheme! (obs-peek @scheme-selection))
                                  (@scheme-selection . := . #f))
                                #:enabled? (obs-map @scheme-selection (λ (s) (and s #t)))))
                       (pref-caption "Imports TextMate (.tmTheme) and VS Code (.json) themes")))))
    (pref-group-gap)
    (hpanel
     #:stretch '(#t #f)
     #:alignment '(right center)
     (button "Close" close! #:style '(border))))))

(define (current-date)
  (seconds->date (current-seconds)))

(define (date->string d fmt)
  (define yr (date-year d))
  (define mo (date-month d))
  (define dy (date-day d))
  (format "~a-~a-~a"
          yr
          (if (< mo 10) (format "0~a" mo) mo)
          (if (< dy 10) (format "0~a" dy) dy)))

;; ============================================================================
;; Main window

(define §app
  (window
   #:size '(900 700)
   #:stretch '(#t #t)
   #:title "Camp"
   #:mixin (λ (%)
             ((remember-geometry-mix @main-geometry)
              (dragdrop-mix
               (class %
                 (super-new)
                 (set-main-frame! this)
                 ;; With the main window gone there's no way back to it,
                 ;; so its editor windows go too
                 (define/augment (can-close?)
                   (and (close-editors!) (inner #t can-close?)))))))
   :main-menu
   (vpanel
    (hpanel
     (vpanel #:min-size '(180 #f)
             #:stretch '(#f #t)
             (button "➕ Add Site" on-add-site #:min-size '(150 40) #:enabled? @switching-enabled?)
             :sites-choice
             :folders-table)
     (vpanel
      :toolbar
      :source-docs-table))
    (hpanel
     #:min-size '(#f 180)
     #:stretch '(#t #f)
     (log-output-textbox)))))

;; ============================================================================
;; Entry point

;; framework loads while the app is otherwise idle, or at once for anything
;; that needs it sooner
(define editor-support
  (delay/idle
   (init-appearance!)
   (apply-active-font-slot!)
   ;; framework installs its own (empty) preferences dialog as it loads
   (install-preferences-handler!)))

(define (install-preferences-handler!)
  (application-preferences-handler (λ () (render (?prefs)))))

(define (run-app)
  (application-about-handler (λ () (render (?about))))
  (install-preferences-handler!)
  (init-app-appearance!)
  (begin0
    (render §app)
    (track-folder-selection!)
    ;; low priority: after the window's first paint
    (queue-callback (λ () (thread load-selected-site!)) #f)))
