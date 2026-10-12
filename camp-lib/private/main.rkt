#lang racket/base

(require racket/path
         racket/list
         racket/format
         racket/string
         "rerequire.rkt"
         syntax/modresolve
         pkg/path
         setup/getinfo
         punct/doc
         punct/fetch
         "structs.rkt"
         "collections.rkt"
         "filter.rkt"
         "xref.rkt")

(provide file-path->site-path
         resolve-site-spec
         load-site
         filter-pages
         current-site-info
         current-output-dir
         get-collection
         get-taxonomy-terms
         get-taxonomy-pages
         prev-in
         next-in
         prev
         next
         ;; Pagination
         paginate
         page-link-doc
         pagination-nav)

;; ---------------------------------------------------------------------------
;; Build Parameters

(define current-site-info (make-parameter #f))
(define current-output-dir (make-parameter #f))

;; ---------------------------------------------------------------------------
;; Site Resolution

(define (resolve-site-spec spec)
  (cond
    [(path? spec) (and (file-exists? spec) spec)]
    [(string? spec)
     (if (file-exists? spec)
         (string->path spec)
         (resolve-site-by-collection spec))]
    [(symbol? spec) (resolve-site-by-collection (symbol->string spec))]
    [else #f]))

(define (resolve-site-by-collection name)
  (define all-dirs (find-relevant-directories '(camp-site) 'all-available))
  (for/or ([dir (in-list all-dirs)])
    (define get-info (get-info/full dir))
    (and get-info
         (equal? (get-info 'collection (λ () #f)) name)
         (let ([camp-site (get-info 'camp-site (λ () #f))])
           (and camp-site (build-path dir camp-site))))))

;; ---------------------------------------------------------------------------
;; Package Info Helpers

(define (file-path->site-path file-path)
  (define abs-path (simplify-path (path->complete-path file-path)))
  (define-values (pkg-name subpath) (path->pkg+subpath abs-path))
  (unless pkg-name
    (error 'file-path->site-path
           "file is not in a package: ~a" abs-path))
  ;; Compute package root by stripping subpath from abs-path
  (define pkg-root
    (cond
      [(eq? subpath 'same) abs-path]
      [(path? subpath)
       (define subpath-parts (explode-path subpath))
       (for/fold ([p abs-path]) ([_ (in-list subpath-parts)])
         (simplify-path (build-path p 'up)))]
      [else (error 'file-path->site-path
                   "unexpected subpath value: ~a" subpath)]))
  ;; Get info procedure for this package
  (define get-pkg-info (get-info/full pkg-root))
  (unless get-pkg-info
    (error 'file-path->site-path
           "no info.rkt found in package root: ~a" pkg-root))
  ;; Look up camp-site field
  (define site-mod-path (get-pkg-info 'camp-site (λ () #f)))
  (unless site-mod-path
    (error 'file-path->site-path
           "info.rkt does not define 'camp-site: ~a" pkg-root))
  ;; Resolve the module path relative to the package root
  (simplify-path (build-path pkg-root site-mod-path)))

;; ---------------------------------------------------------------------------
;; Site Loading

(define (load-site mod-path)
  (define resolved
    (cond
      ;; Hash with 'path key (e.g., a book config) - discover site from package
      [(and (hash? mod-path) (hash-has-key? mod-path 'path))
       (file-path->site-path (hash-ref mod-path 'path))]
      [(and (path-string? mod-path) (absolute-path? mod-path))
       (if (path? mod-path) mod-path (string->path mod-path))]
      [(path-string? mod-path)
       (simplify-path (path->complete-path (if (path? mod-path)
                                               mod-path
                                               (string->path mod-path))))]
      [else
       (resolve-module-path mod-path #f)]))
  (define site-root (simplify-path (build-path resolved 'up)))
  (site-cache-root! site-root)
  (rerequire! resolved)
  (define site-config (dynamic-require resolved 'toml))
  (define get-info (get-info/full site-root))
  (define collection-name (and get-info (get-info 'collection (λ () #f))))
  (hash-set* site-config 'root site-root 'racket-collection collection-name))

;; ---------------------------------------------------------------------------
;; Collection Retrieval

(define (get-collection name
                        #:limit [limit #f]
                        #:full-docs? [full-docs? #f]
                        #:include-drafts? [include-drafts? #f])
  (define info (current-site-info))
  (unless info
    (error 'get-collection "no site-info available (not in build context)"))
  (define raw
    (if full-docs?
        (hash-ref (site-info-pages-by-collection info) name '())
        (hash-ref (site-info-page-links-by-collection info) name '())))
  (define filtered
    (if include-drafts?
        raw
        (filter (λ (item)
                  (not (hash-ref (if full-docs?
                                     (document-metas (page-doc item))
                                     (page-link-metas item))
                                 'draft? #f)))
                raw)))
  (define limited
    (if (and limit (> (length filtered) limit))
        (take filtered limit)
        filtered))
  (if full-docs?
      (map page-doc limited)
      limited))

;; ---------------------------------------------------------------------------
;; Taxonomy Functions

(define (get-taxonomy-terms collection-name taxonomy-key)
  (define info (current-site-info))
  (unless info
    (error 'get-taxonomy-terms "no site-info available (not in build context)"))
  (define pages (site-info-pages info))
  (define tax-sym (string->symbol taxonomy-key))
  (define coll-pages
    (filter (λ (p) (equal? (page-collection-name p) collection-name)) pages))
  (define seen (make-hash))
  (define terms
    (for*/list ([p (in-list coll-pages)]
                [term (in-list (normalize-taxonomy-value
                                (meta-ref (page-doc p) tax-sym)))]
                #:unless (hash-has-key? seen term))
      (hash-set! seen term #t)
      term))
  terms)

(define (get-taxonomy-pages collection-name taxonomy-key [term #f])
  (define info (current-site-info))
  (unless info
    (error 'get-taxonomy-pages "no site-info available (not in build context)"))
  (define tax-index (site-info-taxonomy-index info))
  (define coll-taxes (hash-ref tax-index collection-name #f))
  (unless coll-taxes
    (error 'get-taxonomy-pages "collection not found or has no taxonomies: ~a" collection-name))
  (define term-hash (hash-ref coll-taxes taxonomy-key #f))
  (unless term-hash
    (error 'get-taxonomy-pages "taxonomy not found: ~a in collection ~a" taxonomy-key collection-name))
  (if term
      (hash-ref term-hash term '())
      term-hash))


;; ---------------------------------------------------------------------------
;; Page Navigation

(define (page-link-slug pl)
  (hash-ref (page-link-metas pl) 'slug))

(define (prev-in pages current-slug)
  (define normalized-current (normalize-slug current-slug))
  (let loop ([pages pages] [prev #f])
    (cond
      [(null? pages) #f]
      [(equal? (normalize-slug (page-link-slug (car pages))) normalized-current) prev]
      [else (loop (cdr pages) (car pages))])))

(define (next-in pages current-slug)
  (define normalized-current (normalize-slug current-slug))
  (let loop ([pages pages])
    (cond
      [(null? pages) #f]
      [(null? (cdr pages)) #f]
      [(equal? (normalize-slug (page-link-slug (car pages))) normalized-current) (cadr pages)]
      [else (loop (cdr pages))])))

;; ---------------------------------------------------------------------------
;; Context Navigation

(define (resolve-nav-pages info ctx taxonomy-key term)
  (define coll-name (context-collection ctx))
  (cond
    [(not taxonomy-key)
     (hash-ref (site-info-page-links-by-collection info) coll-name '())]
    [(not term)
     (define terms (hash-ref (context-taxonomies ctx) taxonomy-key '()))
     (if (null? terms)
         '()
         (taxonomy-term-pages info coll-name taxonomy-key (car terms)))]
    [else
     (taxonomy-term-pages info coll-name taxonomy-key term)]))

(define (taxonomy-term-pages info coll-name taxonomy-key term)
  (define coll-taxes (hash-ref (site-info-taxonomy-index info) coll-name (hasheq)))
  (define term-hash (hash-ref coll-taxes taxonomy-key (hash)))
  (hash-ref term-hash term '()))

(define (prev ctx [taxonomy-key #f] [term #f])
  (define info (current-site-info))
  (and info
       (prev-in (resolve-nav-pages info ctx taxonomy-key term)
                (context-slug ctx))))

(define (next ctx [taxonomy-key #f] [term #f])
  (define info (current-site-info))
  (and info
       (next-in (resolve-nav-pages info ctx taxonomy-key term)
                (context-slug ctx))))

;; ---------------------------------------------------------------------------
;; Pagination

(define (paginate collection-name
                   #:per-page per-page
                   #:page-slug [page-slug "page"]
                   render-proc)
  (paginated-content collection-name per-page page-slug render-proc))

(define (page-link-doc pl)
  (define info (current-site-info))
  (unless info
    (error 'page-link-doc "no site-info available (not in build context)"))
  (define slug (hash-ref (page-link-metas pl) 'slug #f))
  (unless slug
    (error 'page-link-doc "page-link has no slug in metas"))
  (define normalized (normalize-slug slug))
  (define pg (hash-ref (site-info-page-by-slug info) normalized #f))
  (unless pg
    (error 'page-link-doc "page not found for slug: ~a" slug))
  (page-doc pg))

(define (pagination-nav pag #:always-show? [always-show? #f])
  (define total (pagination-total-pages pag))
  (define page-num (pagination-page-num pag))
  (define prev-url (pagination-prev-url pag))
  (define next-url (pagination-next-url pag))
  (cond
    [(and (not always-show?) (= total 1))
     '()]
    [else
     `(nav ((class "pagination"))
        ,@(if prev-url
              `((a ((href ,prev-url) (class "pagination-prev")) "← Newer"))
              '())
        (span ((class "pagination-info"))
              ,(format "Page ~a of ~a" page-num total))
        ,@(if next-url
              `((a ((href ,next-url) (class "pagination-next")) "Older →"))
              '()))]))
