#lang racket/base

;; Static file server for Camp dev server

(require html-printer
         net/mime-type
         net/url
         racket/format
         racket/match
         racket/path
         racket/string
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in static-files: web-server/dispatchers/dispatch-files)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/response
         web-server/web-server
         "log.rkt")

(provide start-server)

;; ============================================================================
;; Formatted HTML responses

(define (response/html5-xexpr xpr #:code [code 200] #:message [msg #"OK"])
  (response/full code msg (current-seconds) #"text/html; charset=utf-8" null
                 (list (string->bytes/utf-8 (xexpr->html5 xpr #:add-breaks? #t)))))

;; ============================================================================
;; Logging dispatcher

(define (zpad n)
  (cond
    [(= n 0) "00"]
    [(< n 10) (string-append "0" (number->string n))]
    [else (number->string n)]))

(define (simple-time)
  (define now (seconds->date (current-seconds)))
  (format "~a:~a:~a"
          (zpad (date-hour now))
          (zpad (date-minute now))
          (zpad (date-second now))))

(define (apache-log-datetime)
  (define now (seconds->date (current-seconds)))
  (match-define (date* sec min hr day month-num yr _wkday _yrday _dst tz-offset _ns _tzname) now)
  (define month
    (vector-ref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                (sub1 month-num)))
  (format "~a/~a/~a:~a:~a:~a ~a~a~a"
          (zpad day) month yr (zpad hr) (zpad min) (zpad sec)
          (if (negative? tz-offset) "-" "+")
          (zpad (quotient (abs tz-offset) 3600))
          (zpad (/ (remainder (abs tz-offset) 3600) 60))))

(define (format-status-code code)
  (define indicator
    (cond
      [(< code 300) (green "●")]
      [(< code 400) (cyan "●")]
      [(< code 500) (yellow "●")]
      [else (red "●")]))
  (~a indicator " " code))

(define (log-modern req resp)
  (define method (string-upcase (bytes->string/utf-8 (request-method req))))
  (define path (url->string (request-uri req)))
  (define code (response-code resp))
  (log-camp-info "~a ~a ~a ~a"
                 (dim (simple-time))
                 (format-status-code code)
                 (~a method #:min-width 4)
                 path))

(define (log-apache req resp)
  (log-camp-info
   "~a - - [~a] \"~a ~a HTTP/1.1\" ~a -"
   (request-client-ip req)
   (apache-log-datetime)
   (string-upcase (bytes->string/utf-8 (request-method req)))
   (url->string (request-uri req))
   (response-code resp)))

(define (log:make dispatcher #:format [log-format 'modern])
  (define log-fn (if (eq? log-format 'apache) log-apache log-modern))
  (lambda (conn req)
    (define original-handler (current-header-handler))  ; Capture before parameterize
    (with-handlers ([exn:dispatcher? (lambda (e) (next-dispatcher))])
      (parameterize ([current-header-handler
                      (lambda (resp)
                        (define new-resp (original-handler resp))
                        (log-fn req new-resp)
                        new-resp)])
        (dispatcher conn req)))))

;; ============================================================================
;; Directory listings

(define folder-icon
  (~a "data:image/svg+xml;base64,"
      "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjwhRE9DVFl"
      "QRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYX"
      "BoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwM"
      "DAvc3ZnIiBzdHlsZT0iZmlsbDogIzQxODNjNDsiIHZpZXdCb3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBo"
      "ZWlnaHQ9IjE2IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBhdGggZD0iTTEuNzUgMUExLjc1IDEuNzUgMCAwIDA"
      "gMCAyLjc1djEwLjVDMCAxNC4yMTYuNzg0IDE1IDEuNzUgMTVoMTIuNUExLjc1IDEuNzUgMCAwIDAgMTYgMT"
      "MuMjV2LTguNUExLjc1IDEuNzUgMCAwIDAgMTQuMjUgM0g3LjVhLjI1LjI1IDAgMCAxLS4yLS4xbC0uOS0xL"
      "jJDNi4wNyAxLjI2IDUuNTUgMSA1IDFIMS43NVoiPjwvcGF0aD48L3N2Zz4="))

(define file-icon
  (~a "data:image/svg+xml;base64,"
      "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjwhRE9DVFl"
      "QRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYX"
      "BoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwM"
      "DAvc3ZnIiB2aWV3Qm94PSIwIDAgMTYgMTYiIHN0eWxlPSJmaWxsOmN1cnJlbnRjb2xvciIgd2lkdGg9IjE2"
      "IiBoZWlnaHQ9IjE2IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBhdGggZD0iTTIgMS43NUMyIC43ODQgMi43ODQ"
      "gMCAzLjc1IDBoNi41ODZjLjQ2NCAwIC45MDkuMTg0IDEuMjM3LjUxM2wyLjkxNCAyLjkxNGMuMzI5LjMyOC"
      "41MTMuNzczLjUxMyAxLjIzN3Y5LjU4NkExLjc1IDEuNzUgMCAwIDEgMTMuMjUgMTZoLTkuNUExLjc1IDEuN"
      "zUgMCAwIDEgMiAxNC4yNVptMS43NS0uMjVhLjI1LjI1IDAgMCAwLS4yNS4yNXYxMi41YzAgLjEzOC4xMTIu"
      "MjUuMjUuMjVoOS41YS4yNS4yNSAwIDAgMCAuMjUtLjI1VjZoLTIuNzVBMS43NSAxLjc1IDAgMCAxIDkgNC4"
      "yNVYxLjVabTYuNzUuMDYyVjQuMjVjMCAuMTM4LjExMi4yNS4yNS4yNWgyLjY4OGwtLjAxMS0uMDEzLTIuOT"
      "E0LTIuOTE0LS4wMTMtLjAxMVoiPjwvcGF0aD48L3N2Zz4="))

(define css
  (~a
   ;; Base styles
   "*, *::before, *::after { box-sizing: border-box; }\n"
   "body {\n"
   "  font-family: ui-rounded, 'Hiragino Maru Gothic ProN', Quicksand, Comfortaa, "
   "Manjari, 'Arial Rounded MT', 'Arial Rounded MT Bold', Calibri, source-sans-pro, sans-serif;\n"
   "  margin: 0; padding: 24px 32px;\n"
   "  background: linear-gradient(135deg, #f8f6f3 0%, #f0ede8 100%);\n"
   "  min-height: 100vh;\n"
   "  color: #3d3832;\n"
   "  line-height: 1.5;\n"
   "}\n"
   ;; Container
   ".container { max-width: 720px; margin: 0 auto; }\n"
   ;; Header
   "h1 {\n"
   "  font-size: 1.25rem; font-weight: 600;\n"
   "  color: #2d5a45; margin: 0 0 16px 0;\n"
   "  display: flex; align-items: center; gap: 8px;\n"
   "}\n"
   "h1::before {\n"
   "  content: ''; display: inline-block;\n"
   "  width: 6px; height: 6px;\n"
   "  background: #5a8f6e; border-radius: 50%;\n"
   "}\n"
   ;; File list
   "ul {\n"
   "  list-style: none; margin: 0; padding: 0;\n"
   "  background: #fff; border-radius: 8px;\n"
   "  box-shadow: 0 1px 3px rgba(45,50,55,0.06), 0 1px 2px rgba(45,50,55,0.04);\n"
   "  overflow: hidden;\n"
   "}\n"
   "li { border-bottom: 1px solid #edeae5; }\n"
   "li:last-child { border-bottom: none; }\n"
   "li a {\n"
   "  display: flex; align-items: center; gap: 10px;\n"
   "  padding: 10px 14px;\n"
   "  text-decoration: none; color: #3d3832;\n"
   "  transition: background 0.15s ease;\n"
   "}\n"
   "li a:hover { background: #f8f6f3; }\n"
   "li a::before {\n"
   "  content: ''; flex-shrink: 0;\n"
   "  width: 16px; height: 16px;\n"
   "  background-image: url(\"" file-icon "\");\n"
   "  background-size: contain;\n"
   "  background-repeat: no-repeat;\n"
   "  opacity: 0.7;\n"
   "}\n"
   "li.folder a::before { background-image: url(\"" folder-icon "\"); opacity: 1; }\n"
   "li.folder a { color: #2d5a45; font-weight: 500; }\n"
   ;; Error page styles
   ".error-page {\n"
   "  text-align: center; padding: 48px 24px;\n"
   "}\n"
   ".error-code {\n"
   "  font-size: 4rem; font-weight: 700;\n"
   "  color: #c9a86c; margin: 0; line-height: 1;\n"
   "}\n"
   ".error-title {\n"
   "  font-size: 1.25rem; font-weight: 600;\n"
   "  color: #2d5a45; margin: 12px 0 8px 0;\n"
   "}\n"
   ".error-message {\n"
   "  color: #6b635a; margin: 0;\n"
   "  font-size: 0.95rem;\n"
   "}\n"
   ".error-hint {\n"
   "  margin-top: 24px; padding-top: 20px;\n"
   "  border-top: 1px solid #edeae5;\n"
   "  font-size: 0.85rem; color: #8a837a;\n"
   "}\n"
   ".error-hint a {\n"
   "  color: #5a8f6e; text-decoration: none;\n"
   "  font-weight: 500;\n"
   "}\n"
   ".error-hint a:hover { text-decoration: underline; }\n"
   "code {\n"
   "  font-family: ui-monospace, 'SF Mono', Menlo, Monaco, monospace;\n"
   "  background: #f0ede8; padding: 2px 6px;\n"
   "  border-radius: 4px; font-size: 0.9em;\n"
   "}\n"))

(define root-url
  (url #f #f #f #f #t null null #f))

(define up-url
  (url #f #f #f #f #f (list (path/param 'up null)) null #f))

(define (relative-path-url-to-root p)
  (define simple-path (simplify-path p))
  (define rel-path (find-relative-path (current-directory) simple-path))
  (cond
    [(equal? simple-path rel-path) root-url]
    [else
     (define pp
       (for/list ([d (in-list (explode-path rel-path))])
         (path/param (path->string d) null)))
     (url #f #f #f #f #t pp null #f)]))

(define (make-file-link class url text)
  `(li [[class ,class]]
       (a ([href ,(url->string url)])
          ,text)))

(define (files-list path)
  (for/list ([f (directory-list path #:build? #t)])
    (define name (path->string (file-name-from-path f)))
    (define u (relative-path-url-to-root f))
    (define class
      (if (directory-exists? f) "folder" "file"))
    (make-file-link class u name)))

(define (make-template-xexpr title-string body #:error? [error? #f])
  `(html
    (head
     (meta [[charset "utf-8"]])
     (meta [[name "viewport"] [content "width=device-width, initial-scale=1"]])
     (title ,title-string)
     (style ,css))
    (body
     (div [[class "container"]]
          ,@(if error?
                (list body)
                (list `(h1 ,title-string) body))))))

(define (directory-lister:make #:url->path url->path)
  (lift:make
   (lambda (req)
     (define-values (path pieces) (url->path (request-uri req)))
     (unless (directory-exists? path)
       (next-dispatcher))
     (define root-path?
       (match pieces [(list 'same ...) #t] [_ #f]))
     (define title-string
       (~a "Directory of "
           (url->string (request-uri req))))
     (response/html5-xexpr
      (make-template-xexpr title-string
                           `(ul ,@(if root-path?
                                      null
                                      (list (make-file-link "folder" up-url "..")))
                                ,@(files-list path)))))))

;; ============================================================================
;; Static file dispatcher helpers

;; Support mime types for markdown; add utf-8 for text
(define (file-mime-type p)
  (define type
    (if (regexp-match? #rx"[.](md|markdown)$" (~a p))
        #"text/markdown"
        (path-mime-type p)))
  (if (and type (regexp-match? #rx#"^(text/|application/(javascript|json))" type))
      (bytes-append type #"; charset=utf-8")
      type))

(define (path->headers p)
  (cond
    [(string-suffix? (~a p) ".gz")
     (list (header #"Content-Encoding" #"gzip"))]
    [(or (string-suffix? (~a p) ".atom") (string-suffix? (~a p) ".xml"))
     (list (header #"Content-Type" #"application/xml")
           (header #"x-content-type-options" #"nosniff"))]
    [else '()]))

;; ============================================================================
;; 404 Not Found handler

(define (not-found req)
  (define path (url->string (request-uri req)))
  (response/html5-xexpr
   #:code 404
   #:message #"Not Found"
   (make-template-xexpr
    "Page Not Found"
    #:error? #t
    `(div [[class "error-page"]]
          (p [[class "error-code"]] "404")
          (p [[class "error-title"]] "Page not found")
          (p [[class "error-message"]]
             "The path " (code ,path) " doesn't exist.")
          (p [[class "error-hint"]]
             "Check the URL or " (a [[href "/"]] "return home") ".")))))

;; ============================================================================
;; Main entry point

(define (start-server output-folder
                      #:port [port 8000]
                      #:watch? [watch? #t]
                      #:log-format [log-format 'modern])
  (define base-dir
    (if (path? output-folder)
        output-folder
        (string->path output-folder)))

  (unless (directory-exists? base-dir)
    (error 'start-server "output folder does not exist: ~a" base-dir))

  (define server-url (~a "http://localhost:" port))

  (log-camp-info (~a "  " (dim "Serving") " " (path->string (simplify-path base-dir))))
  (log-camp-info (~a "  " (dim "URL") "     " (cyan server-url)))
  (when watch?
    (log-camp-info (~a "  " (dim "Watching for changes..."))))
  (log-camp-info (~a "  " (dim "Press Ctrl+C to stop")))

  (define shutdown-server
    (parameterize ([current-directory base-dir])
      (define url->path/current-dir (make-url->path (current-directory)))
      (serve #:port port
             #:dispatch
             (log:make
              (sequencer:make
               (static-files:make
                #:url->path url->path/current-dir
                #:path->mime-type file-mime-type
                #:path->headers path->headers)
               (directory-lister:make #:url->path url->path/current-dir)
               (lift:make not-found))
              #:format log-format))))

  shutdown-server)
