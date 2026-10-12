#lang racket/base

(require racket/fasl
         racket/format
         racket/list
         racket/file
         racket/match
         racket/path
         "rerequire.rkt"
         racket/set
         racket/string
         punct/doc
         punct/fetch
         gregor
         html-printer
         "structs.rkt"
         "collections.rkt"
         "dates.rkt"
         "filter.rkt"
         "path-map.rkt"
         "log.rkt"
         "xref.rkt"
         "main.rkt"
         "feeds.rkt")

(provide collect
         collect/call-with-page
         build!
         copy-static-files
         sync-static-files
         build-context
         doc-output-path
         output-path->url)  ; re-exported from collections.rkt

;; ---------------------------------------------------------------------------
;; Collect Pass

(define (collect site)
  (define root-dir (site-root site))
  (define source-ext (site-sources site))
  (define collections (site-collections site))

  (define all-pages
    (parameterize ([current-site-timezone (site-timezone site)])
      (for/fold ([pages '()])
                ([coll (in-list collections)])
        (append pages (collect-collection root-dir source-ext coll)))))

  (define page-index (build-page-index all-pages))
  (define term-index (build-term-index all-pages))
  (define taxonomy-index (build-taxonomy-index all-pages collections))

  (define pages-by-collection
    (let ([grouped (for/fold ([h (hash)])
                             ([p (in-list all-pages)])
                     (hash-update h (page-collection-name p)
                                  (λ (lst) (cons p lst)) '()))])
      ;; Reverse to maintain sort order (pages were consed in reverse)
      (for/hash ([(k v) (in-hash grouped)])
        (values k (reverse v)))))

  (define page-links-by-collection
    (for/hash ([(coll-name pages) (in-hash pages-by-collection)])
      (values coll-name (map page->page-link pages))))

  (define page-by-slug
    (for/fold ([h (hash)])
              ([p (in-list all-pages)])
      (define slug (normalize-slug (page-slug p)))
      (when (hash-has-key? h slug)
        (log-camp-warning "duplicate slug \"~a\": ~a supersedes ~a"
                          slug
                          (page-source-path p)
                          (page-source-path (hash-ref h slug))))
      (hash-set h slug p)))

  (site-info all-pages term-index page-index taxonomy-index
             pages-by-collection page-links-by-collection page-by-slug))

(define (collect/call-with-page site slug proc)
  (define info (collect site))
  (define normalized (normalize-slug slug))
  (define pg (hash-ref (site-info-page-by-slug info) normalized
                       (λ () (error 'collect/call-with-page
                                    "no page found with slug: ~a" slug))))
  (define ctx (build-context pg
                             (page-collection-name pg)
                             (site-info-taxonomy-index info)
                             (site-root site)))
  (parameterize ([current-site-info info]
                 [current-output-dir (site-output-dir site)]
                 [current-site-timezone (site-timezone site)])
    (proc (page-doc pg) ctx)))

;; ---------------------------------------------------------------------------
;; Output Path Helpers

(define (site-output-dir site)
  (build-path (site-root site) (site-output-folder site)))

(define (normalize-output-path path-str)
  (define without-leading
    (if (string-prefix? path-str "/")
        (substring path-str 1)
        path-str))
  (define with-index
    (if (or (string=? without-leading "") (string-suffix? without-leading "/"))
        (string-append without-leading "index.html")
        without-leading))
  (string->path with-index))

;; ---------------------------------------------------------------------------
;; Collection Processing

(define (collect-collection site-root source-ext coll)
  (define coll-name (collection-name coll))
  (define source-pattern (collection-source coll))
  (define output-pattern (collection-output-paths coll))

  (define source-dir
    (build-path site-root (source-pattern->directory source-pattern)))

  (define raw-pages
    (for/list ([(path slug doc) (in-sources source-dir source-ext)])
      (list path slug doc)))

  (define sorted-pages (sort-pages raw-pages coll))

  (define pages
    (for/list ([raw-page (in-list sorted-pages)])
      (match-define (list source-path slug doc) raw-page)
      (page source-path (doc-output-path doc slug output-pattern) doc slug coll-name)))

  (define seen (make-hash))
  (for ([p (in-list pages)])
    (define op (page-output-path p))
    (define existing (hash-ref seen op #f))
    (when existing
      (error 'collect-collection
             "duplicate output path ~a in collection ~s\n  ~a\n  ~a"
             op coll-name
             (page-source-path existing)
             (page-source-path p)))
    (hash-set! seen op p))

  pages)

(define (doc-output-path doc slug output-pattern)
  (define override (meta-ref doc 'output-path))
  (if override
      (normalize-output-path override)
      (format-output-path output-pattern slug (get-page-date doc) (document-metas doc))))

(define (get-page-date doc)
  (define raw-date (meta-ref doc 'date))
  (and raw-date (meta->moment raw-date)))

;; ---------------------------------------------------------------------------
;; Page Index

(define (build-page-index pages)
  (for/hash ([p (in-list pages)])
    (define slug (page-slug p))
    (define normalized-slug (normalize-slug slug))
    (define doc (page-doc p))
    (define title (or (meta-ref doc 'title) slug))
    (define url (output-path->url (page-output-path p)))
    (define metas (hash-set (document-metas doc) 'slug slug))
    (values normalized-slug (page-link url title metas))))

;; ---------------------------------------------------------------------------
;; Term Index

(define (build-term-index pages)
  (for/fold ([index (hash)])
            ([p (in-list pages)])
    (define doc (page-doc p))
    (define terms-defined (meta-ref doc 'terms-defined))
    (define page-url (output-path->url (page-output-path p)))
    (cond
      [(not terms-defined) index]
      [(list? terms-defined)
       (for/fold ([idx index])
                 ([term (in-list terms-defined)])
         (define normalized (normalize-term-name term))
         (when (hash-has-key? idx normalized)
           (log-camp-warning "~a: duplicate term definition: ~a (previously defined elsewhere)"
                             (page-source-path p) term))
         (hash-set idx normalized (string-append page-url "#term-" normalized)))]
      [else index])))

;; ---------------------------------------------------------------------------
;; Taxonomy Index

(define (build-taxonomy-index pages collections)
  (define pages-by-collection
    (for/fold ([grouped (hash)])
              ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (hash-update grouped coll-name (λ (lst) (cons p lst)) '())))

  (define ordered-pages-by-collection
    (for/hash ([(coll-name pgs) (in-hash pages-by-collection)])
      (values coll-name (reverse pgs))))

  (for/fold ([index (hash)])
            ([coll (in-list collections)])
    (define coll-name (collection-name coll))
    (define taxonomies (collection-taxonomies coll))
    (cond
      [(null? taxonomies) index]
      [else
       (define coll-pages (hash-ref ordered-pages-by-collection coll-name '()))
       (define coll-tax-index
         (build-collection-taxonomy-index coll-pages taxonomies))
       (hash-set index coll-name coll-tax-index)])))

(define (build-collection-taxonomy-index pages taxonomies)
  (for/hash ([tax-key (in-list taxonomies)])
    (values tax-key (build-single-taxonomy-index pages tax-key))))

(define (build-single-taxonomy-index pages tax-key)
  (define tax-sym (string->symbol tax-key))
  (for/fold ([index (hash)])
            ([p (in-list pages)])
    (define doc (page-doc p))
    (define raw-val (meta-ref doc tax-sym))
    (define terms (normalize-taxonomy-value raw-val))
    (define pl (page->page-link p))
    (for/fold ([idx index])
              ([term (in-list terms)])
      (hash-update idx term (λ (lst) (append lst (list pl))) '()))))

;; ---------------------------------------------------------------------------
;; Static File Copying

(define (copy-file-preserving-mtime src dest)
  (define mtime (file-or-directory-modify-seconds src))
  (copy-file src dest #:exists-ok? #t)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (log-camp-warning "failed to preserve timestamp for ~a: ~a"
                                       dest (exn-message e)))])
    (file-or-directory-modify-seconds dest mtime)))

(define (copy-static-files source-dir dest-dir)
  (when (directory-exists? source-dir)
    (make-directory* dest-dir)
    (for ([item (in-directory source-dir)])
      (define relative (find-relative-path source-dir item))
      (define dest-path (build-path dest-dir relative))
      (cond
        [(directory-exists? item)
         (make-directory* dest-path)]
        [(file-exists? item)
         (make-parent-directory* dest-path)
         (copy-file-preserving-mtime item dest-path)]))))

;; ---------------------------------------------------------------------------
;; Static File Sync

(define (enumerate-static-files source-dir)
  (if (not (directory-exists? source-dir))
      '()
      (for/list ([item (in-directory source-dir)]
                 #:when (file-exists? item))
        (path->string (find-relative-path source-dir item)))))

(define (read-manifest manifest-path)
  (if (and manifest-path
           (file-exists? manifest-path)
           (> (file-size manifest-path) 0))
      (call-with-input-file manifest-path fasl->s-exp)
      '()))

(define (write-manifest manifest-path paths)
  (when manifest-path
    (call-with-output-file manifest-path
      (λ (out) (s-exp->fasl paths out))
      #:exists 'replace)))

(define (sync-static-files source-dir dest-dir manifest-path)
  (cond
    [(not (directory-exists? source-dir))
     (log-camp-info "static folder does not exist: ~a" source-dir)
     0]
    [else
     (define current-files (enumerate-static-files source-dir))
     (define old-files (read-manifest manifest-path))

     (define current-set (list->set current-files))
     (define files-to-delete
       (filter (λ (f) (not (set-member? current-set f))) old-files))

     (for ([rel-path (in-list files-to-delete)])
       (define dest-path (build-path dest-dir rel-path))
       (when (file-exists? dest-path)
         (delete-file dest-path)
         (log-camp-debug "deleted orphaned static file: ~a" rel-path)))

     (cleanup-empty-directories dest-dir files-to-delete)

     (make-directory* dest-dir)
     (for ([item (in-directory source-dir)])
       (define relative (find-relative-path source-dir item))
       (define dest-path (build-path dest-dir relative))
       (cond
         [(directory-exists? item)
          (make-directory* dest-path)]
         [(file-exists? item)
          (make-parent-directory* dest-path)
          (copy-file-preserving-mtime item dest-path)]))

     (write-manifest manifest-path current-files)

     (length current-files)]))

(define (cleanup-empty-directories base-dir deleted-files)
  (define parent-dirs
    (remove-duplicates
     (filter-map
      (λ (f)
        (define p (path-only f))
        (and p
             (let ([ps (path->string p)])
               (and (not (string=? ps ""))
                    (not (string=? ps "."))
                    ps))))
      deleted-files)))
  (define sorted-dirs
    (sort parent-dirs > #:key (λ (p) (length (explode-path (string->path p))))))
  (for ([dir-str (in-list sorted-dirs)])
    (define full-path (build-path base-dir dir-str))
    (when (and (directory-exists? full-path)
               (null? (directory-list full-path)))
      (delete-directory full-path))))

;; ---------------------------------------------------------------------------
;; Build Pass

(define (build! site info)
  (define output-dir (site-output-dir site))
  (parameterize ([current-site-info info]
                 [current-output-dir output-dir]
                 [current-site-timezone (site-timezone site)])
    (define root-dir (site-root site))
    (define static-dir (build-path root-dir (site-static-folder site)))
    (define collections (site-collections site))
    (define taxonomy-index (site-info-taxonomy-index info))
    (define pages (site-info-pages info))
    (define page-links-by-coll (site-info-page-links-by-collection info))

    (copy-static-files static-dir output-dir)
    (when (directory-exists? static-dir)
      (log-camp-debug "copied static files"))

    (define coll-by-name
      (for/hasheq ([c (in-list collections)])
        (values (collection-name c) c)))

    ;; Build all contexts first so they can be reused for feed generation
    (define contexts-by-slug
      (for/hash ([p (in-list pages)])
        (define coll-name (page-collection-name p))
        (define ctx (build-context p coll-name taxonomy-index root-dir))
        (values (page-slug p) ctx)))

    ;; Render pages using stored contexts
    (define generated-files (mutable-set))
    (define page-count 0)
    (for ([p (in-list pages)])
      (define coll-name (page-collection-name p))
      (define coll (hash-ref coll-by-name coll-name))
      (define render-fn (resolve-render-function coll site))
      (define ctx (hash-ref contexts-by-slug (page-slug p)))
      (define doc (page-doc p))

      ;; Check for paginated camp/page
      (define body-thunk (meta-ref doc 'camp-page-body-thunk))
      (define pagination-result
        (and body-thunk
             (let ([result (body-thunk)])
               (and (paginated-content? result) result))))

      (cond
        [pagination-result
         ;; Build paginated pages
         (set! page-count
               (+ page-count
                  (build-paginated-page! p ctx pagination-result render-fn output-dir info
                                         generated-files)))]
        [else
         ;; Normal page rendering
         (define html-xexpr (render-fn doc ctx))
         (define output-path (build-path output-dir (page-output-path p)))
         (make-parent-directory* output-path)
         (define html-string (xexpr->html5 html-xexpr))
         (call-with-output-file output-path
           (λ (out) (display html-string out))
           #:exists 'replace)
         (set-add! generated-files (simplify-path output-path))
         (set! page-count (add1 page-count))]))

    (log-camp-debug "built ~a pages" page-count)

    (generate-feeds! site info contexts-by-slug)

    ;; Remove orphaned HTML files from previous builds. Static files are copied to the top of the
    ;; output folder, so an output file came from static/ when the same relative path exists there.
    (define (under-static? p)
      (and (directory-exists? static-dir)
           (file-exists? (build-path static-dir (find-relative-path output-dir p)))))
    (define deleted-rel-paths
      (for/list ([p (in-directory output-dir)]
                 #:when (and (regexp-match? #rx"\\.html$" (path->string p))
                             (not (set-member? generated-files (simplify-path p)))
                             (not (under-static? p))))
        (delete-file p)
        (define rel (find-relative-path output-dir p))
        (log-camp-debug "deleted orphaned page: ~a" rel)
        (path->string rel)))
    (unless (null? deleted-rel-paths)
      (cleanup-empty-directories output-dir deleted-rel-paths))))

;; ---------------------------------------------------------------------------
;; Paginated Page Building

(define (build-paginated-page! page base-ctx pc render-fn output-dir info generated-files)
  (define coll-name (paginated-content-collection-name pc))
  (define per-page (paginated-content-per-page pc))
  (define page-slug-str (paginated-content-page-slug pc))
  (define render-proc (paginated-content-render-proc pc))

  (define all-items
    (hash-ref (site-info-page-links-by-collection info) coll-name '()))
  (define total-items (length all-items))
  (define total-pages (max 1 (ceiling (/ total-items per-page))))

  (define base-output (page-output-path page))
  (define base-url (output-path->url base-output))
  (define original-doc (page-doc page))
  (define original-title (or (meta-ref original-doc 'title) ""))
  (define original-metas (document-metas original-doc))

  (for ([page-num (in-range 1 (add1 total-pages))])
    (define start (* (sub1 page-num) per-page))
    (define items
      (take (drop all-items (min start total-items))
            (min per-page (max 0 (- total-items start)))))

    ;; Calculate output path for this page
    (define output-path
      (if (= page-num 1)
          (build-path output-dir base-output)
          (let* ([base-dir (path-only base-output)]
                 [dir-path (if base-dir
                               (build-path output-dir base-dir page-slug-str
                                           (number->string page-num))
                               (build-path output-dir page-slug-str
                                           (number->string page-num)))])
            (build-path dir-path "index.html"))))

    ;; Calculate URLs
    (define current-url
      (if (= page-num 1)
          base-url
          (string-append base-url page-slug-str "/" (number->string page-num) "/")))

    (define prev-url
      (cond
        [(= page-num 1) #f]
        [(= page-num 2) base-url]
        [else (string-append base-url page-slug-str "/" (number->string (sub1 page-num)) "/")]))

    (define next-url
      (if (< page-num total-pages)
          (string-append base-url page-slug-str "/" (number->string (add1 page-num)) "/")
          #f))

    ;; Create pagination context
    (define pag
      (pagination page-num total-pages total-items base-url current-url prev-url next-url))

    ;; Call user's render proc to get body content
    (define body-content (render-proc items pag))

    ;; Create page title (append page number for pages 2+)
    (define page-title
      (if (= page-num 1)
          original-title
          (format "~a - Page ~a" original-title page-num)))

    ;; Create synthetic doc with modified title and cached body
    (define synthetic-metas
      (hash-set* original-metas
                 'title page-title
                 'camp-page-body-thunk (λ () body-content)))
    (define synthetic-doc
      (document synthetic-metas '() '()))

    ;; Build context for this page
    (define ctx
      (hash-set* base-ctx
                 'slug (if (= page-num 1)
                           (page-slug page)
                           (format "~a/~a/~a" (page-slug page) page-slug-str page-num))
                 'url current-url
                 'taxonomies (hash)))

    ;; Render through normal render function
    (define html-xexpr (render-fn synthetic-doc ctx))
    (make-parent-directory* output-path)
    (define html-string (xexpr->html5 html-xexpr))
    (call-with-output-file output-path
      (λ (out) (display html-string out))
      #:exists 'replace)
    (set-add! generated-files (simplify-path output-path)))

  total-pages)

;; ---------------------------------------------------------------------------
;; Render Function Resolution

(define (resolve-render-function coll site)
  (define render-spec (or (collection-render-with coll)
                          (site-default-render site)))
  (unless render-spec
    (error 'build! "no render function for collection ~a and no site default"
           (collection-name coll)))
  (rerequire! (car render-spec))
  (apply dynamic-require render-spec))

;; ---------------------------------------------------------------------------
;; Context Building

(define (build-context p coll-name taxonomy-index root)
  (define doc (page-doc p))
  (context (page-slug p)
           (output-path->url (page-output-path p))
           coll-name
           (build-page-taxonomies doc taxonomy-index coll-name)
           (source-path->string root (page-source-path p))))

(define (source-path->string root path)
  (string-join (map path-element->string (explode-path (find-relative-path root path)))
               "/"))

(define (build-page-taxonomies doc taxonomy-index coll-name)
  (define coll-taxes (hash-ref taxonomy-index coll-name #f))
  (if (not coll-taxes)
      (hash)
      (for/hash ([(tax-key _term-hash) (in-hash coll-taxes)])
        (define raw-val (meta-ref doc (string->symbol tax-key)))
        (values tax-key (normalize-taxonomy-value raw-val)))))
