#lang racket/base

;; Date metas: parsing to moments in the site timezone

(require gregor
         racket/string
         splitflap/constructs)

(provide current-site-timezone
         meta->moment)

(define current-site-timezone (make-parameter #f))

(define (meta->moment v)
  (define site-tz (current-site-timezone))
  (define tz (or site-tz (current-timezone)))
  (define m
    (cond
      [(moment? v) v]
      [(datetime? v) (with-timezone v tz)]
      [(date? v) (with-timezone (at-midnight v) tz)]
      [(non-empty-string? v) (parameterize ([current-timezone tz]) (infer-moment v))]
      [else (raise-argument-error 'meta->moment
                                  "(or/c moment? datetime? date? non-empty-string?)"
                                  v)]))
  (if site-tz (adjust-timezone m site-tz) m))
