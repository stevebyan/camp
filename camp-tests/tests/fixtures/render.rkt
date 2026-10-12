#lang racket/base

;; Minimal render functions for testing

(require camp
         racket/file
         racket/format)

(provide feed-content
         render-page
         render-post
         render-with-asset)

(define (feed-content doc context)
  `(p "Feed content for " ,(context-url context)))

(define (render-page doc context)
  `(html (body ,@(camp-doc->html-xexpr doc))))

(define (render-post doc context)
  `(html (body ,@(camp-doc->html-xexpr doc))))

(define (render-with-asset doc context)
  (define asset
    (build-path (current-output-dir) "assets" (~a (context-slug context) ".txt")))
  (make-parent-directory* asset)
  (display-to-file (context-slug context) asset #:exists 'replace)
  (render-page doc context))
