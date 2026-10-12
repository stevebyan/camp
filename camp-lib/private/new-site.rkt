#lang racket/base

;; Site template generation using include-template

(require racket/date
         racket/format
         racket/path
         racket/file
         racket/runtime-path
         racket/string
         web-server/templates)

(define-runtime-path template-static-dir "template/static")

(provide create-new-site)

;; Template generation functions
;; Each function takes the template variables and returns the file content

(define (gen-info.rkt name)
  (include-template "template/info.rkt.txt"))

(define (gen-site.rkt name title author founded)
  (include-template "template/site.rkt.txt"))

(define (gen-render.rkt title)
  (include-template "template/render.rkt.txt"))

(define (gen-main.rkt name)
  (include-template "template/main.rkt.txt"))

(define (gen-index.md.rkt name)
  (include-template "template/pages/index.md.rkt.txt"))

(define (gen-feeds.rkt)
  (include-template "template/feeds.rkt.txt"))

(define (gen-first-post.md.rkt name founded)
  (include-template "template/blog/first-post.md.rkt.txt"))

(define (gen-welcome.md.rkt name earlier-date)
  (include-template "template/blog/welcome.md.rkt.txt"))

;; Create a new site in the given directory
(define (create-new-site target-dir name)
  (define title (name->title name))
  (define author "Author (author@example.com)")
  (define founded (today-date-string))
  (define earlier-date (days-ago-string 3))

  ;; Create directory structure
  (make-directory* target-dir)
  (make-directory* (build-path target-dir "blog"))
  (make-directory* (build-path target-dir "pages"))
  (make-directory* (build-path target-dir "static"))

  ;; Write templated files
  (call-with-output-file (build-path target-dir "info.rkt")
    (λ (out) (display (gen-info.rkt name) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "site.rkt")
    (λ (out) (display (gen-site.rkt name title author founded) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "render.rkt")
    (λ (out) (display (gen-render.rkt title) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "main.rkt")
    (λ (out) (display (gen-main.rkt name) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "pages" "index.md.rkt")
    (λ (out) (display (gen-index.md.rkt name) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "feeds.rkt")
    (λ (out) (display (gen-feeds.rkt) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "blog" "first-post.md.rkt")
    (λ (out) (display (gen-first-post.md.rkt name founded) out))
    #:exists 'error)

  (call-with-output-file (build-path target-dir "blog" "welcome.md.rkt")
    (λ (out) (display (gen-welcome.md.rkt name earlier-date) out))
    #:exists 'error)

  ;; Copy static files (no templating needed)
  (copy-file (build-path template-static-dir "style.css")
             (build-path target-dir "static" "style.css")))

;; Convert kebab-case name to title case
(define (name->title name)
  (string-join
   (for/list ([word (regexp-split #rx"-" name)])
     (if (> (string-length word) 0)
         (string-append (string-upcase (substring word 0 1))
                        (substring word 1))
         word))
   " "))

;; Get today's date in TOML format
(define (today-date-string)
  (define d (current-date))
  (format "~a-~a-~a"
          (date-year d)
          (~a (date-month d) #:min-width 2 #:pad-string "0" #:align 'right)
          (~a (date-day d) #:min-width 2 #:pad-string "0" #:align 'right)))

;; Get a date N days ago in TOML format
(define (days-ago-string n)
  (define secs (- (current-seconds) (* n 24 60 60)))
  (define d (seconds->date secs))
  (format "~a-~a-~a"
          (date-year d)
          (~a (date-month d) #:min-width 2 #:pad-string "0" #:align 'right)
          (~a (date-day d) #:min-width 2 #:pad-string "0" #:align 'right)))
