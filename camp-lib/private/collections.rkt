#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/path
         "rerequire.rkt"
         racket/string
         punct/fetch
         punct/doc
         gregor
         "dates.rkt"
         "structs.rkt")

(provide source-pattern->directory
         is-source?
         find-sources
         in-sources
         load-doc
         path->slug
         parse-sort-value
         sort-pages
         output-path->url
         page->page-link)

;; ---------------------------------------------------------------------------
;; Source Pattern Handling

(define (source-pattern->directory source-pattern)
  (define p (string->path source-pattern))
  (define parts (explode-path p))
  (if (= 1 (length parts))
      (current-directory)
      (apply build-path (drop-right parts 1))))

;; ---------------------------------------------------------------------------
;; Source File Detection

(define (is-source? p source-extension)
  (define path-bytes (if (path? p) (path->bytes p) p))
  (and (or (path-has-extension? p (string->bytes/utf-8 source-extension))
           (path-has-extension? p #".page.rkt")
           (path-has-extension? p #".md.rkt"))
       (not (regexp-match? #rx#"\\.#" path-bytes))))

;; ---------------------------------------------------------------------------
;; Source File Discovery

(define (find-sources root source-extension)
  (for/list ([p (in-list (directory-list root #:build? #t))]
             #:when (and (file-exists? p)
                         (is-source? p source-extension)))
    p))

;; ---------------------------------------------------------------------------
;; Slug Extraction

(define (path->slug p source-extension)
  (define as-path (if (path? p) p (string->path p)))
  (define filename (path->string (file-name-from-path as-path)))
  (define ext-pattern
    (string-append "(?:" (regexp-quote source-extension)
                   "|" (regexp-quote ".page.rkt")
                   "|" (regexp-quote ".md.rkt")
                   ")$"))
  (regexp-replace (regexp ext-pattern) filename ""))

;; ---------------------------------------------------------------------------
;; Source Loading

;; Unlike punct's get-doc, loads through rerequire! so later edits to the
;; source or its dependencies are picked up within a long-running process. A
;; source module first loaded any other way can never be reloaded in that
;; process, so all doc loading must go through this function.
(define (load-doc p)
  (rerequire! p)
  (dynamic-require p 'doc))

;; ---------------------------------------------------------------------------
;; Source Iteration

(define (in-sources root source-extension)
  (define paths (find-sources root source-extension))
  (make-do-sequence
   (lambda ()
     (values
      (lambda (idx)
        (define p (list-ref paths idx))
        (define doc (load-doc p))
        (define slug (or (meta-ref doc 'slug)
                         (path->slug p source-extension)))
        (values p slug doc))
      add1
      0
      (lambda (idx) (< idx (length paths)))
      #f
      #f))))

;; ---------------------------------------------------------------------------
;; Page URL and Link Construction

(define (output-path->url output-path)
  (define path-str (path->string output-path))
  (define normalized (string-replace path-str "\\" "/"))
  (define clean (regexp-replace #rx"index\\.html$" normalized ""))
  (if (string-prefix? clean "/")
      clean
      (string-append "/" clean)))

(define (page->page-link p)
  (define doc (page-doc p))
  (define slug (page-slug p))
  (define title (or (meta-ref doc 'title) slug))
  (define url (output-path->url (page-output-path p)))
  (define metas (hash-set (document-metas doc) 'slug slug))
  (page-link url title metas))

;; ---------------------------------------------------------------------------
;; Page Sorting

(define (parse-sort-value val sort-key)
  (if (equal? sort-key "date") (meta->moment val) val))

(define (sort-pages pages coll)
  (define sort-key (collection-sort-key coll))
  (define order (collection-order coll))
  (define coll-name (collection-name coll))

  (define pages-with-sort-vals
    (for/list ([page (in-list pages)])
      (match-define (list _ slug doc) page)
      (define raw-val (meta-ref doc (string->symbol sort-key)))
      (unless raw-val
        (error 'sort-pages
               "page \"~a\" in collection \"~a\" is missing required metadata key \"~a\""
               slug coll-name sort-key))
      (define sort-val (parse-sort-value raw-val sort-key))
      (cons sort-val page)))

  (define ascending? (equal? order "ascending"))

  (define (compare-values a b)
    (define (less-than? x y)
      (cond
        [(and (moment? x) (moment? y)) (moment<? x y)]
        [(and (date? x) (date? y)) (date<? x y)]
        [(and (string? x) (string? y)) (string<? x y)]
        [(and (number? x) (number? y)) (< x y)]
        [else (string<? (format "~a" x) (format "~a" y))]))
    (if ascending?
        (less-than? a b)
        (less-than? b a)))

  (define sorted (sort pages-with-sort-vals compare-values #:key car))
  (map cdr sorted))
