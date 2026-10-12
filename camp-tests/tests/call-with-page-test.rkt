#lang racket/base

(require rackunit
         racket/path
         punct/doc
         punct/fetch
         camp
         camp/build)

;; ---------------------------------------------------------------------------
;; Test fixture paths

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(define fixture-site-path
  (build-path fixture-site-root "site.rkt"))

(define (load-test-site)
  (load-site fixture-site-path))

;; ===========================================================================
;; collect/call-with-page tests
;; ===========================================================================

(test-case "collect/call-with-page: invokes proc with correct arguments"
  (define site (load-test-site))
  (define result
    (collect/call-with-page site "first-post"
      (λ (doc ctx)
        (check-pred document? doc)
        (check-pred context? ctx)
        (check-equal? (context-slug ctx) "first-post")
        (check-equal? (context-collection ctx) "blog")
        (check-equal? (meta-ref doc 'title) "First Post")
        'ok)))
  (check-equal? result 'ok))

(test-case "collect/call-with-page: site-info is available inside proc"
  (define site (load-test-site))
  (collect/call-with-page site "first-post"
    (λ (doc ctx)
      (check-not-false (current-site-info)))))

(test-case "collect/call-with-page: errors on nonexistent slug"
  (define site (load-test-site))
  (check-exn
   #rx"no page found with slug"
   (λ () (collect/call-with-page site "nonexistent-slug" void))))

(test-case "collect/call-with-page: normalizes slug casing"
  (define site (load-test-site))
  (collect/call-with-page site "First-Post"
    (λ (doc ctx)
      (check-equal? (meta-ref doc 'title) "First Post"))))

(test-case "collect/call-with-page: source is site-relative with / separators"
  (define site (load-test-site))
  (collect/call-with-page site "custom-slug"
    (λ (doc ctx)
      (check-equal? (context-source ctx) "blog/third-post.md.rkt")))
  (collect/call-with-page site "about"
    (λ (doc ctx)
      (check-equal? (context-source ctx) "pages/about.md.rkt"))))
