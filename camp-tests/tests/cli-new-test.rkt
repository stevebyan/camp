#lang racket/base

;; Tests for raco camp new command

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         racket/system)

;; ---------------------------------------------------------------------------
;; Helper to run raco camp new

(define (run-new . args)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define result
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (apply system*/exit-code
             (find-executable-path "raco")
             "camp" "new" args)))
  (values result
          (get-output-string stdout)
          (get-output-string stderr)))

;; Use a temporary directory for testing
(define test-dir (make-temporary-file "camp-new-test-~a" 'directory))

;; ---------------------------------------------------------------------------
;; Test: valid site name creates all expected files

(define site-name "my-test-site")
(define site-dir (build-path test-dir site-name))

(parameterize ([current-directory test-dir])
  (define-values (exit-ok out-ok err-ok)
    (run-new site-name))

  (check-equal? exit-ok 0
                "should exit zero on successful create")

  (check-true (directory-exists? site-dir)
              "should create site directory")

  (check-true (file-exists? (build-path site-dir "info.rkt"))
              "should create info.rkt")

  (check-true (file-exists? (build-path site-dir "site.rkt"))
              "should create site.rkt")

  (check-true (file-exists? (build-path site-dir "render.rkt"))
              "should create render.rkt")

  (check-true (file-exists? (build-path site-dir "pages" "index.md.rkt"))
              "should create pages/index.md.rkt")

  (check-true (file-exists? (build-path site-dir "static" "style.css"))
              "should create static/style.css")

  (check-true (file-exists? (build-path site-dir "feeds.rkt"))
              "should create feeds.rkt")

  (check-true (file-exists? (build-path site-dir "blog" "first-post.md.rkt"))
              "should create blog/first-post.md.rkt")

  (check-true (file-exists? (build-path site-dir "blog" "welcome.md.rkt"))
              "should create blog/welcome.md.rkt")

  (check-true (file-exists? (build-path site-dir "main.rkt"))
              "should create main.rkt")

  ;; Check info.rkt has correct collection name
  (define info-content (file->string (build-path site-dir "info.rkt")))
  (check-regexp-match (regexp (format "collection \"~a\"" site-name))
                      info-content
                      "info.rkt should have correct collection name")

  ;; Check site.rkt has correct render-with module path
  (define site-content (file->string (build-path site-dir "site.rkt")))
  (check-regexp-match (regexp (format "~a/render" site-name))
                      site-content
                      "site.rkt should reference render module")

  ;; Check main.rkt provides camp bindings
  (define main-content (file->string (build-path site-dir "main.rkt")))
  (check-regexp-match #rx"all-from-out camp"
                      main-content
                      "main.rkt should re-export camp")

  ;; Check source files use #lang punct with site name
  (define post-content (file->string (build-path site-dir "blog" "first-post.md.rkt")))
  (check-regexp-match (regexp (format "#lang punct ~a" site-name))
                      post-content
                      "blog posts should use #lang punct <sitename>")

  (define page-content (file->string (build-path site-dir "pages" "index.md.rkt")))
  (check-regexp-match (regexp (format "#lang punct ~a" site-name))
                      page-content
                      "pages should use #lang punct <sitename>")

  ;; Check the starter posts showcase cross-references
  (define welcome-content (file->string (build-path site-dir "blog" "welcome.md.rkt")))
  (check-regexp-match #rx"•defterm{" welcome-content
                      "welcome post should define a term")
  (check-regexp-match #rx"•term{" post-content
                      "first post should reference a term")
  (check-regexp-match #rx"•page-ref\\[" post-content
                      "first post should reference a page")

  ;; Check render.rkt shows the site title
  (define render-content (file->string (build-path site-dir "render.rkt")))
  (check-regexp-match #rx"\"My Test Site\"" render-content
                      "render.rkt should contain the site title")

  ;; Check style.css styles every cross-reference hook
  (define css-content (file->string (build-path site-dir "static" "style.css")))
  (for ([hook '("dfn.term-def" "a.term-ref" ".unresolved-ref" "dfn.term-def:target")])
    (check-true (string-contains? css-content hook)
                (format "style.css should style ~a" hook))))

;; ---------------------------------------------------------------------------
;; Test: the new site builds without warnings, and its references resolve

(define (run-build)
  (define output (open-output-string))
  (define result
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (putenv "PLTCOLLECTS" (format "~a:" test-dir))
      (begin0
        (system*/exit-code (find-executable-path "raco")
                           "camp" "build" "--drama"
                           (build-path site-dir "site.rkt"))
        (environment-variables-set! (current-environment-variables)
                                    #"PLTCOLLECTS" #f))))
  (values result (get-output-string output)))

(let-values ([(exit-build out-build) (run-build)])
  (check-equal? exit-build 0
                (format "new site should build without warnings:\n~a" out-build))

  (define built-posts
    (for/list ([f (in-directory site-dir)]
               #:when (regexp-match? #rx"first-post/index\\.html$" (path->string f)))
      (file->string f)))
  (check-equal? (length built-posts) 1
                "should build the first post")
  (for ([html built-posts])
    (check-regexp-match #rx"class=\"term-ref\"" html
                        "built post should link to a term definition")
    (check-regexp-match #rx"class=\"page-ref\"" html
                        "built post should link to a page")
    (check-false (string-contains? html "unresolved-ref")
                 "built post should have no unresolved references")))

;; ---------------------------------------------------------------------------
;; Test: existing directory produces error

(parameterize ([current-directory test-dir])
  (define-values (exit-exists out-exists err-exists)
    (run-new site-name))

  (check-not-equal? exit-exists 0
                    "should exit non-zero when directory exists")

  (check-regexp-match #rx"already exists"
                      err-exists
                      "error should mention directory exists"))

;; ---------------------------------------------------------------------------
;; Test: invalid name produces error

(parameterize ([current-directory test-dir])
  (define-values (exit-invalid out-invalid err-invalid)
    (run-new "Invalid Name"))

  (check-not-equal? exit-invalid 0
                    "should exit non-zero for invalid name")

  (check-regexp-match #rx"Invalid site name"
                      err-invalid
                      "error should mention invalid name"))

;; ---------------------------------------------------------------------------
;; Test: name starting with number produces error

(parameterize ([current-directory test-dir])
  (define-values (exit-num out-num err-num)
    (run-new "123site"))

  (check-not-equal? exit-num 0
                    "should exit non-zero for name starting with number")

  (check-regexp-match #rx"Invalid site name"
                      err-num
                      "error should mention invalid name"))

;; ---------------------------------------------------------------------------
;; Cleanup

(delete-directory/files test-dir)
