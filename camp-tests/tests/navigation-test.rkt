#lang racket/base

;; Tests for prev/next navigation functions

(require rackunit
         racket/list
         racket/path
         (except-in camp prev next)
         camp/build
         (only-in camp/private/main prev next))

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-site-path
  (build-path fixture-site-root "site.rkt"))

;; ---------------------------------------------------------------------------
;; Unit tests with mock site-info

(define pl-a (page-link "/a/" "A" (hasheq 'slug "a")))
(define pl-b (page-link "/b/" "B" (hasheq 'slug "b")))
(define pl-c (page-link "/c/" "C" (hasheq 'slug "c")))

(define mock-info
  (site-info '() ; pages
             (hasheq "blog" (hasheq "tags" (hasheq "alpha" (list pl-a pl-b)
                                                   "beta"  (list pl-b pl-c))))
             (hasheq) ; page-index
             (hasheq "blog" (hasheq "tags" (hasheq "alpha" (list pl-a pl-b)
                                                   "beta"  (list pl-b pl-c))))
             (hasheq "blog" (list pl-a pl-b pl-c))  ; pages-by-collection
             (hasheq "blog" (list pl-a pl-b pl-c))  ; page-links-by-collection
             (hasheq)))                              ; page-by-slug

(define (make-ctx slug #:taxonomies [taxonomies (hasheq)])
  (hasheq 'slug slug
          'url (string-append "/" slug "/")
          'collection "blog"
          'taxonomies taxonomies))

(test-case "prev: returns previous page in collection"
  (parameterize ([current-site-info mock-info])
    (check-equal? (prev (make-ctx "b")) pl-a)
    (check-equal? (prev (make-ctx "c")) pl-b)))

(test-case "prev: returns #f at start of collection"
  (parameterize ([current-site-info mock-info])
    (check-false (prev (make-ctx "a")))))

(test-case "next: returns next page in collection"
  (parameterize ([current-site-info mock-info])
    (check-equal? (next (make-ctx "a")) pl-b)
    (check-equal? (next (make-ctx "b")) pl-c)))

(test-case "next: returns #f at end of collection"
  (parameterize ([current-site-info mock-info])
    (check-false (next (make-ctx "c")))))

(test-case "prev: navigates within taxonomy using first term"
  (parameterize ([current-site-info mock-info])
    ;; b has tags alpha, beta; first is alpha. Within alpha: a, b
    (define ctx (make-ctx "b" #:taxonomies (hasheq "tags" '("alpha" "beta"))))
    (check-equal? (prev ctx "tags") pl-a)))

(test-case "next: navigates within taxonomy using first term"
  (parameterize ([current-site-info mock-info])
    ;; b has tags beta, alpha; first is beta. Within beta: b, c
    (define ctx (make-ctx "b" #:taxonomies (hasheq "tags" '("beta" "alpha"))))
    (check-equal? (next ctx "tags") pl-c)))

(test-case "prev: navigates within specific taxonomy term"
  (parameterize ([current-site-info mock-info])
    ;; Within beta: b, c. prev of c in beta = b
    (define ctx (make-ctx "c" #:taxonomies (hasheq "tags" '("beta"))))
    (check-equal? (prev ctx "tags" "beta") pl-b)))

(test-case "next: navigates within specific taxonomy term"
  (parameterize ([current-site-info mock-info])
    ;; Within alpha: a, b. next of a in alpha = b
    (define ctx (make-ctx "a" #:taxonomies (hasheq "tags" '("alpha"))))
    (check-equal? (next ctx "tags" "alpha") pl-b)))

(test-case "prev/next: returns #f when no site-info available"
  (parameterize ([current-site-info #f])
    (check-false (prev (make-ctx "b")))
    (check-false (next (make-ctx "b")))))

(test-case "prev/next: returns #f for unknown taxonomy"
  (parameterize ([current-site-info mock-info])
    (define ctx (make-ctx "b" #:taxonomies (hasheq)))
    (check-false (prev ctx "nonexistent"))
    (check-false (next ctx "nonexistent"))))

;; ---------------------------------------------------------------------------
;; Integration tests with real site fixture

(define (call-with-built-context target-slug thunk)
  (define site (load-site fixture-site-path))
  (define info (collect site))
  (parameterize ([current-site-info info])
    (define pg
      (for/or ([p (site-info-pages info)])
        (and (equal? (page-slug p) target-slug) p)))
    (unless pg
      (error 'call-with-built-context "page not found: ~a" target-slug))
    (define coll-name (page-collection-name pg))
    (define taxonomy-index (site-info-taxonomy-index info))
    (define ctx ((dynamic-require 'camp/private/build 'build-context)
                 pg coll-name taxonomy-index (site-root site)))
    (thunk ctx)))

(test-case "integration: prev returns previous page in collection"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; Blog is descending by date: draft, third (custom-slug), second, first
      ;; So prev of second-post should be custom-slug (Third Post)
      (define p (prev ctx))
      (check-pred page-link? p)
      (check-equal? (page-link-title p) "Third Post"))))

(test-case "integration: next returns next page in collection"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; next of second-post should be first-post
      (define n (next ctx))
      (check-pred page-link? n)
      (check-equal? (page-link-title n) "First Post"))))

(test-case "integration: prev returns #f for first page in collection"
  (call-with-built-context "draft-post"
    (λ (ctx)
      ;; draft-post is first in descending order
      (check-false (prev ctx)))))

(test-case "integration: next returns #f for last page in collection"
  (call-with-built-context "first-post"
    (λ (ctx)
      ;; first-post is last in descending order
      (check-false (next ctx)))))

(test-case "integration: prev with taxonomy key navigates within taxonomy"
  (call-with-built-context "first-post"
    (λ (ctx)
      ;; first-post has tags: alpha, beta (first tag is alpha)
      ;; Within "alpha" tag (descending): draft-post, custom-slug, first-post
      ;; So prev within alpha should be custom-slug (Third Post)
      (define p (prev ctx "tags"))
      (check-pred page-link? p)
      (check-equal? (page-link-title p) "Third Post"))))

(test-case "integration: next with taxonomy key navigates within taxonomy"
  (call-with-built-context "second-post"
    (λ (ctx)
      ;; second-post has tags: beta, gamma (first tag is beta)
      ;; Within "beta" tag (descending): second-post, first-post
      ;; So next within beta should be first-post
      (define n (next ctx "tags"))
      (check-pred page-link? n)
      (check-equal? (page-link-title n) "First Post"))))
