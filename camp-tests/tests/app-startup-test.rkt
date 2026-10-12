#lang racket/base

;; App startup: the main window must be able to appear before the selected
;; site loads and before the libraries only later actions need (framework for
;; the editor, the web server for previews). Loading the app module itself
;; must therefore neither load the site nor instantiate those libraries.
;;
;; Instantiation is process-global, so the check runs in a racket subprocess
;; whose PLTUSERHOME points at a scratch directory with a site selected.

(require racket/file
         racket/port
         racket/runtime-path
         racket/system
         rackunit
         camp/app/private/subprocess-env)

(define-runtime-path fixture-site "fixtures/test-site")

(define racket-bin (find-system-path 'exec-file))

(define (run-sandboxed home program)
  (define env (racket-subprocess-env))
  (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
  (parameterize ([current-environment-variables env])
    (with-output-to-string
      (λ () (check-true (system* racket-bin "-e" (format "~s" program)))))))

(define home (make-temporary-directory "camp-app-startup-~a"))
(define site-dir (build-path home "site"))
(copy-directory/files fixture-site site-dir)
(define site-file (path->string (build-path site-dir "site.rkt")))

(define pref-dir
  (string->path
   (run-sandboxed home '(display (find-system-path 'pref-dir)))))
(make-directory* pref-dir)
(put-preferences '(sites site-selection) (list (list site-file) site-file)
                 #f (build-path pref-dir "camp-prefs.rktd"))

(define declared
  (read
   (open-input-string
    (run-sandboxed
     home
     `(begin
        (require camp/app/private/app)
        (write (for/list ([m (list 'framework
                                   'drracket-vim-tool/private/text
                                   'web-server/dispatchers/dispatch
                                   (string->path ,site-file))])
                 (cons (format "~a" m) (module-declared? m #f)))))))))

(for ([d (in-list declared)])
  (check-false (cdr d) (format "loading the app module must not load ~a" (car d))))

(delete-directory/files home)
