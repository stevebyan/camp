#lang racket/base

;; Book Build Pipeline
;;
;; Builds PDF books from Camp book configurations via Typst.
;; The user provides a render procedure that receives gathered parts/chapters
;; and returns a complete Typst document string.

(require racket/file
         racket/list
         racket/path
         racket/port
         "rerequire.rkt"
         racket/system
         punct/doc
         punct/fetch
         "structs.rkt"
         "dates.rkt"
         "main.rkt"
         "build.rkt"
         "filter.rkt"
         "xref.rkt"
         "log.rkt")

(provide gather-book-parts
         copy-includes!
         build-book!
         load-book)

;; ---------------------------------------------------------------------------
;; Book Loading

(define (load-book file-path)
  (define abs-path
    (simplify-path
     (path->complete-path
      (if (path? file-path) file-path (string->path file-path)))))
  (rerequire! abs-path)
  (define book-config (dynamic-require abs-path 'toml))
  (hash-set book-config 'path abs-path))

;; ---------------------------------------------------------------------------
;; Chapter Construction

(define (make-chapter source-path doc)
  (define raw-slug (or (meta-ref doc 'slug)
                       (path->slug source-path)))
  (hasheq 'slug (normalize-slug raw-slug)
          'doc doc))

(define (path->slug p)
  (define as-path (if (path? p) p (string->path p)))
  (define filename (file-name-from-path as-path))
  ;; Remove extensions (handles both .rkt and .md.rkt, .poly.pm, etc.)
  (define without-ext (path-replace-extension filename #""))
  (define result (path-replace-extension without-ext #""))
  (path->string result))

;; ---------------------------------------------------------------------------
;; Part Gathering

(define (gather-book-parts book site site-info)
  (define root-dir (site-root site))
  (define parts-config (book-parts book))

  (for/list ([part-cfg (in-list parts-config)])
    (define part-name (book-part-name part-cfg))
    (define chapters (gather-part-chapters part-cfg root-dir site site-info))
    (hasheq 'name part-name
            'chapters chapters)))

(define (gather-part-chapters part-cfg root-dir site site-info)
  (define explicit-pages (book-part-pages part-cfg))
  (define collection-names (book-part-collections part-cfg))

  ;; Gather explicit pages first
  (define explicit-chapters
    (for/list ([page-path (in-list explicit-pages)])
      (load-chapter-from-path root-dir page-path)))

  ;; Then gather filtered collection pages
  (define collection-chapters
    (gather-collection-chapters part-cfg collection-names site-info))

  (append explicit-chapters collection-chapters))

(define (load-chapter-from-path root-dir rel-path)
  (define full-path (simplify-path (build-path root-dir rel-path)))
  (define doc (get-doc full-path))
  (make-chapter full-path doc))

(define (gather-collection-chapters part-cfg collection-names site-info)
  (define date-from (book-part-date-from part-cfg))
  (define date-to (book-part-date-to part-cfg))
  (define taxonomies (book-part-taxonomies part-cfg))
  (define page-by-slug (site-info-page-by-slug site-info))

  (for*/list ([coll-name (in-list collection-names)]
              [ch (in-list (gather-single-collection coll-name date-from date-to
                                                      taxonomies site-info page-by-slug))])
    ch))

(define (gather-single-collection coll-name date-from date-to taxonomies site-info page-by-slug)
  (define page-links (hash-ref (site-info-page-links-by-collection site-info) coll-name #f))
  (unless page-links
    (error 'gather-book-parts "collection not found: ~a" coll-name))

  ;; Apply filters
  (define filtered
    (filter-pages page-links
                  #:date-from date-from
                  #:date-to date-to
                  #:taxonomies taxonomies))

  ;; Convert page-links back to chapters
  (for/list ([pl (in-list filtered)])
    (define slug (hash-ref (page-link-metas pl) 'slug))
    (define p (hash-ref page-by-slug (normalize-slug slug) #f))
    (unless p
      (error 'gather-book-parts "page not found for slug: ~a" slug))
    (make-chapter (page-source-path p) (page-doc p))))

;; ---------------------------------------------------------------------------
;; Includes Copying

(define (copy-includes! includes root-dir output-dir)
  (for ([include (in-list includes)])
    (define src (build-path root-dir include))
    (define dest (build-path output-dir include))
    (cond
      [(file-exists? src)
       (make-parent-directory* dest)
       (copy-file src dest #t)]
      [(directory-exists? src)
       (copy-directory/files src dest #:keep-modify-seconds? #t)]
      [else
       (error 'copy-includes! "include not found: ~a" include)])))

;; ---------------------------------------------------------------------------
;; Typst Compilation

(define (compile-typst! input-path output-path root-path)
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f
                (find-executable-path "typst")
                "compile"
                "--root" (path->string root-path)
                (path->string input-path)
                (path->string output-path)))

  (close-output-port stdin)
  (define out-str (port->string stdout))
  (define err-str (port->string stderr))
  (close-input-port stdout)
  (close-input-port stderr)

  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))

  (values exit-code out-str err-str))

;; ---------------------------------------------------------------------------
;; Main Build Function

(define (build-book! book)
  (define the-book-path (book-path book))

  ;; Load and collect site
  (define site (load-site book))
  (define site-info (collect site))

  ;; Set up paths
  (define root-dir (site-root site))
  (define output-dir (build-path root-dir (book-output-folder book)))

  ;; Derive book slug from filename
  (define book-filename (file-name-from-path the-book-path))
  (define book-slug
    (regexp-replace #rx"\\.book\\.rkt$"
                    (path->string book-filename)
                    ""))

  (define typst-output (build-path output-dir (string-append book-slug ".typ")))
  (define pdf-output (build-path output-dir (string-append book-slug ".pdf")))

  ;; Ensure output directory exists
  (make-directory* output-dir)

  ;; Copy includes
  (define includes (book-includes book))
  (unless (null? includes)
    (log-camp-info "copying includes...")
    (copy-includes! includes root-dir output-dir))

  ;; Gather parts and chapters
  (log-camp-info "gathering book parts...")
  (define parts
    (parameterize ([current-site-info site-info]
                   [current-site-timezone (site-timezone site)])
      (gather-book-parts book site site-info)))

  ;; Load and call the user's render function
  (log-camp-info "rendering book content...")
  (define render-fn (apply dynamic-require (book-render-with book)))

  (define rendered-content
    (parameterize ([current-site-info site-info]
                   [current-site-timezone (site-timezone site)])
      (render-fn parts)))

  ;; Write Typst file
  (log-camp-info "writing typst file...")
  (call-with-output-file typst-output
    (λ (out) (display rendered-content out))
    #:exists 'replace)

  ;; Compile with Typst
  (log-camp-info "compiling PDF...")
  (define-values (exit-code stdout stderr) (compile-typst! typst-output pdf-output root-dir))

  (cond
    [(= exit-code 0)
     (log-camp-info "book built: ~a" pdf-output)
     pdf-output]
    [else
     (log-camp-error "typst compilation failed (exit ~a)" exit-code)
     (unless (string=? stderr "")
       (log-camp-error "~a" stderr))
     #f]))
