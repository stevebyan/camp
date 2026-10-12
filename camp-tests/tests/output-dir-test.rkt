#lang racket/base

;; Tests for the current-output-dir parameter

(require rackunit
         racket/file
         racket/path
         compiler/cm
         camp
         camp/build)

(define fixture-site-root
  (simplify-path
   (build-path (path-only (syntax-source #'here))
               "fixtures" "test-site")))

(test-case "current-output-dir: #f outside a build"
  (check-false (current-output-dir)))

(test-case "current-output-dir: set by collect/call-with-page"
  (define site (load-site (build-path fixture-site-root "site.rkt")))
  (collect/call-with-page site "first-post"
    (λ (doc ctx)
      (check-equal? (current-output-dir)
                    (build-path fixture-site-root "publish"))))
  (check-false (current-output-dir)))

(define (call-with-asset-test-site proc)
  (define temp-dir (make-temporary-file "camp-output-dir-test-~a" 'directory))
  (dynamic-wind
    void
    (λ ()
      (define site-rkt (build-path temp-dir "site.rkt"))
      (define post (build-path temp-dir "blog" "hello.md.rkt"))
      (make-parent-directory* post)
      (display-lines-to-file
       '("#lang camp/site"
         ""
         "title = \"Output Dir Test Site\""
         "url = \"https://test.example.com\""
         "founded = 2024-01-01"
         "authors = [\"Test (test@example.com)\"]"
         "output-folder = \"out\""
         "default-render = '(camp/tests/fixtures/render render-with-asset)'"
         ""
         "[[collections]]"
         "name = \"blog\""
         "source = \"blog/*\""
         "output-paths = \"blog/*/\"")
       site-rkt)
      (display-lines-to-file
       '("#lang punct"
         "---"
         "title: Hello"
         "date: 2024-01-10"
         "---"
         "Hello.")
       post)
      (managed-compile-zo post)
      (proc (load-site site-rkt) (build-path temp-dir "out")))
    (λ () (delete-directory/files temp-dir))))

(test-case "current-output-dir: render functions can write files that survive the build"
  (call-with-asset-test-site
   (λ (site output-dir)
     (build! site (collect site))
     (check-true (file-exists? (build-path output-dir "blog" "hello" "index.html")))
     (check-equal? (file->string (build-path output-dir "assets" "hello.txt"))
                   "hello")
     (check-false (current-output-dir)))))
