#lang racket/base

;; Tests for feed generation functionality

(require rackunit
         racket/string
         racket/match
         racket/list
         racket/path
         gregor
         splitflap
         camp
         camp/build
         camp/private/feeds
         camp/private/structs)

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

;; ---------------------------------------------------------------------------
;; parse-author tests

;; Person structs aren't directly comparable with equal?, so check predicate
(check-pred person? (parse-author "Marian Paroo (marian@example.com)")
            "Should parse simple author string to person")

(check-pred person? (parse-author "John Q. Public (john.q.public@example.org)")
            "Should handle complex names and emails")

(check-pred person? (parse-author "Single (single@test.com)")
            "Should handle single-word names")

;; ---------------------------------------------------------------------------
;; make-feed-tag-uri tests

;; Create a mock site config for testing
(define test-site-url "https://test.example.com")
(define test-founded (date 2024 1 15))

(define single-coll-tag
  (make-feed-tag-uri test-site-url test-founded '("blog")))

(check-pred tag-uri? single-coll-tag
            "Should produce a valid tag URI")

(check-true (string-contains? (tag-uri->string single-coll-tag) "test.example.com")
            "Tag URI should contain domain")

(check-true (string-contains? (tag-uri->string single-coll-tag) "2024")
            "Tag URI should contain year")

(check-true (string-contains? (tag-uri->string single-coll-tag) "blog")
            "Tag URI should contain collection name")

;; Test with multiple collections
(define multi-coll-tag
  (make-feed-tag-uri test-site-url test-founded '("blog" "news")))

(check-true (string-contains? (tag-uri->string multi-coll-tag) "blog")
            "Multi-collection tag should contain first collection")

(check-true (string-contains? (tag-uri->string multi-coll-tag) "news")
            "Multi-collection tag should contain second collection")

;; ---------------------------------------------------------------------------
;; filter-feed-pages tests

;; Create mock page-links for testing
(define (make-test-page-link slug title date-str [draft? #f])
  (page-link
   (string-append "/" slug "/")
   title
   (hash 'slug slug
         'title title
         'date (if date-str date-str #f)
         'draft? draft?)))

(define test-pages
  (list (make-test-page-link "post-1" "Post 1" "2024-01-15")
        (make-test-page-link "post-2" "Post 2" "2024-02-15")
        (make-test-page-link "draft" "Draft Post" "2024-03-15" #t)
        (make-test-page-link "no-date" "No Date Post" #f)))

(define filtered (filter-feed-pages test-pages))

(check-equal? (length filtered) 2
              "Should exclude drafts and pages without dates")

(check-true (andmap (lambda (p)
                      (not (equal? "draft" (hash-ref (page-link-metas p) 'slug))))
                    filtered)
            "Should not include draft pages")

(check-true (andmap (lambda (p)
                      (define date-val (hash-ref (page-link-metas p) 'date #f))
                      (and date-val (not (eq? date-val #f))))
                    filtered)
            "All filtered pages should have dates")

;; ---------------------------------------------------------------------------
;; page-link->feed-item tests
;; (Tested via integration test below since it requires site-info context)

;; ---------------------------------------------------------------------------
;; Integration: generate-feed tests

;; Load the test site
(define test-site (load-site (build-path fixture-site-root "site.rkt")))
(define test-info (collect test-site))

;; Build contexts for feed generation (mirrors what build! does)
(define test-contexts-by-slug
  (let ([taxonomy-index (site-info-taxonomy-index test-info)]
        [page-links-by-coll (site-info-page-links-by-collection test-info)])
    (for/hash ([p (in-list (site-info-pages test-info))])
      (define coll-name (page-collection-name p))
      (define slug (page-slug p))
      (define url (string-append "/" (regexp-replace #rx"index\\.html$"
                                                      (path->string (page-output-path p))
                                                      "")))
      (define coll-pages (hash-ref page-links-by-coll coll-name '()))
      (values slug (hasheq 'slug slug
                           'url url
                           'collection coll-name
                           'prev (λ args #f)
                           'next (λ args #f)
                           'taxonomies (hash))))))

;; Generate feed XML
(define feed-xml
  (parameterize ([current-site-info test-info])
    (generate-feed test-site test-info (car (site-feeds test-site)) test-contexts-by-slug)))

(check-pred string? feed-xml
            "Should produce a string of XML")

(check-true (string-contains? feed-xml "<?xml")
            "Should be valid XML with declaration")

(check-true (string-contains? feed-xml "<feed")
            "Atom feed should have <feed> element")

(check-true (string-contains? feed-xml "Test Site")
            "Feed should contain site title")

;; Should contain entries for non-draft posts
(check-true (string-contains? feed-xml "<entry>")
            "Feed should have entries")

(check-true (string-contains? feed-xml "First Post")
            "Feed should contain first post")

;; Draft posts should be excluded (they don't have entries)
(check-false (string-contains? feed-xml "Draft Post")
             "Feed should NOT contain draft posts")

;; Pages collection posts shouldn't be in blog feed
(check-false (string-contains? feed-xml "About")
             "Blog feed should NOT contain pages")

;; ---------------------------------------------------------------------------
;; page-link->feed-item: published and updated timestamps

(define (timestamps-for metas)
  (define item
    (parameterize ([current-site-info test-info]
                   [current-timezone 0])
      (page-link->feed-item (page-link "/dated/" "Dated" (hash-set metas 'slug "dated"))
                            single-coll-tag
                            test-site-url
                            (parse-author "Test (test@example.com)")
                            (λ (doc ctx) '(p))
                            (hash))))
  (define entry (express-xml item 'atom #:as 'xexpr))
  (for/list ([tag '(published updated)])
    (cadr (assq tag (cddr entry)))))

(check-equal? (timestamps-for (hash 'date "2024-02-15"))
              '("2024-02-15T00:00:00Z" "2024-02-15T00:00:00Z")
              "Without an updated meta, updated equals published")

(check-equal? (timestamps-for (hash 'date "2024-02-15" 'updated "2024-02-20T08:30"))
              '("2024-02-15T00:00:00Z" "2024-02-20T08:30:00Z")
              "An updated meta supplies the updated timestamp")

(check-equal? (timestamps-for (hash 'date "2024-02-15 09:00" 'updated "2024-02-15 09:00"))
              '("2024-02-15T09:00:00Z" "2024-02-15T09:00:00Z")
              "An updated meta may equal the date")

(check-exn #rx"dated"
           (λ () (timestamps-for (hash 'date "2024-02-15" 'updated "2024-02-14")))
           "An updated meta before the date raises an error naming the page")

(check-exn exn:fail:contract?
           (λ () (timestamps-for (hash 'date "2024-02-15" 'updated "")))
           "An empty updated meta raises an error")

;; ---------------------------------------------------------------------------
;; Multi-collection feeds: entry IDs derive from each entry's own collection

(define (entry-ids xml)
  ;; all <id> contents except the first, which is the feed's own <id>
  (cdr (regexp-match* #px"<id>([^<]+)</id>" xml #:match-select cadr)))

(define multi-feed-config
  (hasheq 'filename "everything.atom"
          'collections '("blog" "pages")
          'render-with '(camp/tests/fixtures/render feed-content)))

(define multi-xml
  (parameterize ([current-site-info test-info])
    (generate-feed test-site test-info multi-feed-config test-contexts-by-slug)))

(check-equal? (filter (λ (id) (regexp-match? #px":blog\\." id)) (entry-ids multi-xml))
              (entry-ids feed-xml)
              "Entries keep the same IDs when their collection joins a multi-collection feed")

(check-true (for/or ([id (in-list (entry-ids multi-xml))])
              (regexp-match? #px":pages\\." id))
            "Entries from the second collection get IDs under their own collection")

(check-false (for/or ([id (in-list (entry-ids multi-xml))])
               (string-contains? id "blog,pages"))
             "No entry ID should use the feed's joined collection names")

;; ---------------------------------------------------------------------------
;; RSS feed extension test

;; Create an RSS feed config (using .rss extension)
(define rss-feed-config
  (hasheq 'filename "feed.rss"
          'collections '("blog")
          'render-with '(camp/tests/fixtures/render feed-content)))

(define rss-xml
  (parameterize ([current-site-info test-info])
    (generate-feed test-site test-info rss-feed-config test-contexts-by-slug)))

(check-true (string-contains? rss-xml "<rss")
            "RSS feed should have <rss> element")

(check-true (string-contains? rss-xml "<channel>")
            "RSS feed should have <channel> element")

(check-true (string-contains? rss-xml "<item>")
            "RSS feed should have <item> elements")
