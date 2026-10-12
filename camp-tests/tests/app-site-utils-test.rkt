#lang racket/base

;; Tests for camp/app/private/site-utils (page listing entries)

(require rackunit
         racket/path
         camp/app/private/site-utils)

(define fixture-site
  (load-site
   (simplify-path
    (build-path (path-only (syntax-source #'here))
                "fixtures" "test-site" "site.rkt"))))

(define entries
  (get-pages-in-folder fixture-site (build-path (site-root fixture-site) "blog")))

(define (entry-for filename)
  (for/first ([e (in-vector entries)]
              #:when (equal? (path->string (file-name-from-path (vector-ref e 4))) filename))
    e))

;; ---------------------------------------------------------------------------
;; Draft status column

(check-equal? (vector-ref (entry-for "draft-post.md.rkt") 2) "Draft")
(check-equal? (vector-ref (entry-for "first-post.md.rkt") 2) "")
