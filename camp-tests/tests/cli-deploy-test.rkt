#lang racket/base

;; Tests for raco camp deploy command

(require rackunit
         racket/file
         racket/path
         racket/port
         racket/string
         racket/system)

(define fixtures-dir
  (simplify-path
   (build-path (path-only (syntax-source #'here)) "fixtures")))

(define test-site-dir
  (build-path fixtures-dir "test-site"))

(define site-without-deploy
  (build-path fixtures-dir "site.rkt"))

;; ---------------------------------------------------------------------------
;; Helper to run raco camp deploy

(define (run-deploy site-path)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define result
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (system*/exit-code
       (find-executable-path "raco")
       "camp" "deploy" (path->string site-path))))
  (values result
          (get-output-string stdout)
          (get-output-string stderr)))

;; ---------------------------------------------------------------------------
;; Test: missing deploy-script produces error

(define-values (exit-no-script out-no-script err-no-script)
  (run-deploy site-without-deploy))

(check-not-equal? exit-no-script 0
                  "should exit non-zero when deploy-script not configured")
(check-regexp-match #rx"deploy-script"
                    err-no-script
                    "error should mention deploy-script")

;; ---------------------------------------------------------------------------
;; Test: deploy runs script with output folder

(define marker-file
  (build-path test-site-dir "publish" ".deploy-marker"))

;; deploy does not build, and a fresh checkout has no output folder
(make-directory* (path-only marker-file))
(when (file-exists? marker-file)
  (delete-file marker-file))

(define-values (exit-ok out-ok err-ok)
  (run-deploy (build-path test-site-dir "site.rkt")))

(check-equal? exit-ok 0
              "should exit zero on successful deploy")

(check-true (file-exists? marker-file)
            "deploy script should have created marker file")

(define marker-content
  (string-trim (file->string marker-file)))

(define expected-output-dir
  (path->string (build-path test-site-dir "publish")))

(check-equal? marker-content expected-output-dir
              "deploy script should receive output folder as argument")

;; Clean up
(when (file-exists? marker-file)
  (delete-file marker-file))
