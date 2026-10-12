#lang racket/base

(require hash-view)

(provide
 (hash-view-out site)
 (hash-view-out collection)
 (hash-view-out feed-config)
 (struct-out page)
 (struct-out page-link)
 (struct-out site-info)
 (hash-view-out context)
 ;; Pagination
 (struct-out pagination)
 (struct-out paginated-content)
 ;; Book publishing
 (hash-view-out book)
 (hash-view-out book-part)
 (hash-view-out part)
 (hash-view-out chapter))

;; ---------------------------------------------------------------------------
;; Configuration hash-views

(hash-view site
  (title
   url
   founded
   authors
   sources
   static-folder
   output-folder
   collections
   [root #:default #f]                ; site root directory, set by load-site
   [racket-collection #:default #f]   ; package collection name, set by load-site
   [deploy-script #:default #f]
   [default-render #:default #f]
   [timezone #:default #f]
   [feeds #:default '()]))

(hash-view collection
  (name
   source
   output-paths
   [render-with #:default #f]
   [order #:default "descending"]
   [sort-key #:default "date"]
   [taxonomies #:default '()]))

(hash-view feed-config
  (filename
   collections
   render-with))

;; ---------------------------------------------------------------------------
;; Structs

(struct page (source-path output-path doc slug collection-name) #:transparent)
(struct page-link (url title metas) #:transparent)
(struct site-info (pages term-index page-index taxonomy-index
                   pages-by-collection       ; collection-name → (listof page?)
                   page-links-by-collection  ; collection-name → (listof page-link?)
                   page-by-slug)             ; slug → page?
  #:transparent)

;; ---------------------------------------------------------------------------
;; Pagination

(struct pagination
  (page-num        ; positive-integer?
   total-pages     ; positive-integer?
   total-items     ; natural?
   base-url        ; string?
   current-url     ; string?
   prev-url        ; (or/c string? #f)
   next-url)       ; (or/c string? #f)
  #:transparent)

(struct paginated-content
  (collection-name  ; string?
   per-page         ; exact-positive-integer?
   page-slug        ; string?
   render-proc)     ; (-> (listof page-link?) pagination? xexpr?)
  #:transparent)

;; ---------------------------------------------------------------------------
;; Render context

(hash-view context
  (slug
   url
   collection
   taxonomies
   [source #:default #f]))

;; ---------------------------------------------------------------------------
;; Book publishing hash-views

;; Book configuration (from #lang camp/book)
(hash-view book
  (render-with                        ; (list module-path identifier)
   output-folder
   parts
   [path #:default #f]                ; absolute path to book file, set by load-book
   [includes #:default '()]))         ; files/dirs to copy to output before typst

;; Book part configuration (from [[parts]] in book config)
(hash-view book-part
  (name
   [pages #:default '()]
   [collections #:default '()]
   [date-from #:default #f]
   [date-to #:default #f]
   [taxonomies #:default #hasheq()]))

;; Gathered part (passed to render procedure)
(hash-view part
  (name
   chapters))

;; Gathered chapter (passed to render procedure)
(hash-view chapter
  (slug
   doc))
