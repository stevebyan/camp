#lang racket/base

;; Tests for camp main module (site loading, hash-views, navigation)

(require rackunit
         gregor
         racket/path
         camp)

;; ---------------------------------------------------------------------------
;; Collection hash-view tests

(define test-collection
  (collection "blog" "blog/*" "blog/yyyy/MM/*/"))

(check-true (collection? test-collection))
(check-equal? (collection-name test-collection) "blog")
(check-equal? (collection-source test-collection) "blog/*")
(check-equal? (collection-output-paths test-collection) "blog/yyyy/MM/*/")
(check-equal? (collection-render-with test-collection) #f)
(check-equal? (collection-order test-collection) "descending")
(check-equal? (collection-sort-key test-collection) "date")
(check-equal? (collection-taxonomies test-collection) '())

(define test-collection-full
  (collection "blog" "blog/*" "blog/yyyy/MM/*/"
              '(mysite/render render-post) "ascending" "title" '("tags" "series")))

(check-equal? (collection-render-with test-collection-full) '(mysite/render render-post))
(check-equal? (collection-order test-collection-full) "ascending")
(check-equal? (collection-taxonomies test-collection-full) '("tags" "series"))

(check-equal? (hash-ref test-collection 'name) "blog")

;; ---------------------------------------------------------------------------
;; Feed-config hash-view tests

(define test-feed
  (feed-config "feed.atom" '("blog") '(mysite/feeds feed-content)))

(check-true (feed-config? test-feed))
(check-equal? (feed-config-filename test-feed) "feed.atom")
(check-equal? (feed-config-collections test-feed) '("blog"))
(check-equal? (feed-config-render-with test-feed) '(mysite/feeds feed-content))

;; ---------------------------------------------------------------------------
;; Site hash-view tests

(define test-site
  (site "My Blog"
        "https://example.com"
        (date 2020 1 15)
        '("Jane Doe (jane@example.com)")
        ".md.rkt"
        "static"
        "publish"
        (list test-collection)))

(check-true (site? test-site))
(check-equal? (site-title test-site) "My Blog")
(check-equal? (site-url test-site) "https://example.com")
(check-equal? (site-sources test-site) ".md.rkt")
(check-equal? (site-deploy-script test-site) #f)
(check-equal? (site-default-render test-site) #f)
(check-equal? (site-feeds test-site) '())

;; ---------------------------------------------------------------------------
;; Page-link struct tests

(define test-page-link
  (page-link "/blog/2024/01/hello/" "Hello World" (hasheq 'title "Hello World")))

(check-equal? (page-link-url test-page-link) "/blog/2024/01/hello/")
(check-equal? (page-link-title test-page-link) "Hello World")

;; ---------------------------------------------------------------------------
;; Context hash-view tests

(define test-context
  (context "my-post"
           "/blog/2024/01/my-post/"
           "blog"
           (hasheq "tags" '("emacs" "racket"))))

(check-true (context? test-context))
(check-equal? (context-slug test-context) "my-post")
(check-equal? (context-url test-context) "/blog/2024/01/my-post/")
(check-equal? (context-collection test-context) "blog")
(check-equal? (hash-ref (context-taxonomies test-context) "tags") '("emacs" "racket"))

(check-equal? (hash-ref test-context 'slug) "my-post")
(check-equal? (hash-ref test-context 'url) "/blog/2024/01/my-post/")

(check-false (context-source test-context))
(check-equal? (context-source (context "my-post" "/my-post/" "blog" (hasheq)
                                       "blog/my-post.md.rkt"))
              "blog/my-post.md.rkt")

;; ---------------------------------------------------------------------------
;; load-site tests

(define fixture-site-path
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "site.rkt")))

(define loaded-site (load-site fixture-site-path))

(check-true (site? loaded-site) "loaded site should satisfy site? predicate")
(check-equal? (site-title loaded-site) "Test Site")
(check-equal? (site-url loaded-site) "https://test.example.com")
(check-equal? (site-sources loaded-site) ".md.rkt")
(check-equal? (site-static-folder loaded-site) "static")
(check-equal? (site-output-folder loaded-site) "publish")

(check-equal? (site-authors loaded-site) '("Test Author (test@example.com)"))

(check-true (date-provider? (site-founded loaded-site)))

(define collections (site-collections loaded-site))
(check-equal? (length collections) 2)

(define blog-coll (car collections))
(check-true (collection? blog-coll))
(check-equal? (collection-name blog-coll) "blog")
(check-equal? (collection-source blog-coll) "blog/*")
(check-equal? (collection-output-paths blog-coll) "blog/yyyy/MM/*/")
(check-equal? (collection-order blog-coll) "descending")
(check-equal? (collection-sort-key blog-coll) "date")
(check-equal? (collection-taxonomies blog-coll) '("tags" "series"))

(define pages-coll (cadr collections))
(check-equal? (collection-name pages-coll) "pages")
(check-equal? (collection-source pages-coll) "pages/*")
(check-equal? (collection-output-paths pages-coll) "*/")

(define feeds (site-feeds loaded-site))
(check-equal? (length feeds) 1)

(define feed (car feeds))
(check-true (feed-config? feed))
(check-equal? (feed-config-filename feed) "feed.atom")
(check-equal? (feed-config-collections feed) '("blog"))

(check-equal? (hash-ref loaded-site 'title) "Test Site")
(check-equal? (hash-ref loaded-site 'url) "https://test.example.com")

;; load-site should set the root to the directory containing the site config
(define expected-root
  (simplify-path (build-path fixture-site-path 'up)))
(check-equal? (site-root loaded-site) expected-root)
(check-true (directory-exists? (site-root loaded-site)))

;; ---------------------------------------------------------------------------
;; ~d date formatting with auto-conversion (pattern first, like format)

(define test-date (date 2025 1 15))

;; Works with ISO 8601 strings (common from Punct metas)
(check-equal? (~d "d MMM yyyy" "2025-01-15") "15 Jan 2025")
(check-equal? (~d "yyyy-MM-dd" "2025-01-15") "2025-01-15")
(check-equal? (~d "MMMM d, yyyy" "2025-01-15") "January 15, 2025")

;; Also works with gregor date objects
(check-equal? (~d "d MMM yyyy" test-date) "15 Jan 2025")
(check-equal? (~d "EEE, MMM d" (date 2024 12 25)) "Wed, Dec 25")

;; Strings with a time and offset keep them
(check-equal? (~d "yyyy-MM-dd HH:mm" "2025-01-15T09:30") "2025-01-15 09:30")
(check-equal? (~d "HH:mm xxx" "2025-01-15 09:30+05:30") "09:30 +05:30")

;; ---------------------------------------------------------------------------
;; Path pattern predicates and format-output-path (exported from camp)

;; source-path-pattern?
(check-true (source-path-pattern? "blog/*"))
(check-true (source-path-pattern? "*"))
(check-false (source-path-pattern? "/absolute/*"))
(check-false (source-path-pattern? "no-wildcard"))

;; output-path-pattern?
(check-true (output-path-pattern? "blog/*/"))
(check-true (output-path-pattern? "blog/[yyyy]/[MM]/*/"))
(check-false (output-path-pattern? "no-wildcard"))
(check-true (output-path-pattern? "blog/[invalid]/*/"))

;; file-extension?
(check-true (file-extension? ".md.rkt"))
(check-true (file-extension? ".txt"))
(check-false (file-extension? "no-dot"))

;; non-rkt-file-extension?
(check-true (non-rkt-file-extension? ".md.rkt"))
(check-false (non-rkt-file-extension? ".rkt"))

;; format-output-path
(check-equal? (format-output-path "posts/*/" "hello" #f #f)
              (string->path "posts/hello/index.html"))
(check-equal? (format-output-path "blog/[yyyy]/[MM]/*/" "my-post" test-date #f)
              (string->path "blog/2025/01/my-post/index.html"))
