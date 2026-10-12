#lang racket/base

;; Tests for date meta parsing and the site timezone setting

(require rackunit
         racket/file
         racket/list
         gregor
         camp
         camp/build
         camp/private/dates
         camp/private/structs)

(define-syntax-rule (in-chicago body ...)
  (parameterize ([current-timezone "America/Chicago"]) body ...))

;; ---------------------------------------------------------------------------
;; meta->moment without a site timezone

(test-case "meta->moment: date-only string is midnight in current-timezone"
  (in-chicago
   (check-equal? (meta->moment "2024-03-05")
                 (moment 2024 3 5 #:tz "America/Chicago"))))

(test-case "meta->moment: accepts T or space before the time"
  (in-chicago
   (define expected (moment 2024 3 5 10 30 #:tz "America/Chicago"))
   (check-equal? (meta->moment "2024-03-05T10:30") expected)
   (check-equal? (meta->moment "2024-03-05 10:30") expected)))

(test-case "meta->moment: an explicit offset is kept"
  (in-chicago
   (check-equal? (meta->moment "2024-03-05T10:30:00-08:00")
                 (moment 2024 3 5 10 30 #:tz -28800))))

(test-case "meta->moment: gregor values"
  (in-chicago
   (define m (moment 2024 3 5 10 30 #:tz "Europe/Paris"))
   (check-equal? (meta->moment m) m)
   (check-equal? (meta->moment (date 2024 3 5))
                 (moment 2024 3 5 #:tz "America/Chicago"))
   (check-equal? (meta->moment (datetime 2024 3 5 10 30))
                 (moment 2024 3 5 10 30 #:tz "America/Chicago"))))

(test-case "meta->moment: rejects empty, malformed and non-date values"
  (for ([bad (in-list (list "" "yesterday" "2024-03-05 10:30 CST" 42 #f))])
    (check-exn exn:fail:contract? (λ () (meta->moment bad)) (format "~v" bad))))

;; ---------------------------------------------------------------------------
;; meta->moment with a site timezone

(define-syntax-rule (in-tokyo-site body ...)
  (in-chicago (parameterize ([current-site-timezone "Asia/Tokyo"]) body ...)))

(test-case "meta->moment: site timezone applies where no offset is given"
  (in-tokyo-site
   (check-equal? (meta->moment "2024-03-05T10:30")
                 (moment 2024 3 5 10 30 #:tz "Asia/Tokyo"))
   (check-equal? (meta->moment (date 2024 3 5))
                 (moment 2024 3 5 #:tz "Asia/Tokyo"))
   (check-equal? (meta->moment (datetime 2024 3 5 10 30))
                 (moment 2024 3 5 10 30 #:tz "Asia/Tokyo"))))

(test-case "meta->moment: explicit offsets are expressed in the site timezone"
  (in-tokyo-site
   (define given (moment 2024 3 5 23 30 #:tz -28800))
   (for ([v (list "2024-03-05T23:30-08:00" given)])
     (define m (meta->moment v))
     (check-true (moment=? m given))
     (check-equal? (->timezone m) "Asia/Tokyo")
     (check-equal? (->date m) (date 2024 3 6)))))

;; ---------------------------------------------------------------------------
;; filter-pages

(define (dated-link date-val)
  (page-link "/p/" "P" (hasheq 'date date-val)))

(test-case "filter-pages: compares dates with times against date bounds"
  (in-chicago
   (define links (map dated-link '("2024-03-04T23:59" "2024-03-05T10:30" "2024-03-06")))
   (check-equal? (filter-pages links #:date-from (date 2024 3 5) #:date-to (date 2024 3 5))
                 (list (second links)))
   (check-equal? (filter-pages links #:date-from (moment 2024 3 5 18) #:date-to (date 2024 3 5))
                 (list (second links)))))

(test-case "filter-pages: raises on an unparseable date"
  (check-exn exn:fail:contract?
             (λ () (filter-pages (list (dated-link "soon")) #:date-from (date 2024 1 1)))))

;; ---------------------------------------------------------------------------
;; timezone site setting

(define (call-with-temp-site timezone-line proc)
  (define dir (make-temporary-directory))
  (dynamic-wind
   void
   (λ ()
     (define site-rkt (build-path dir "site.rkt"))
     (define post (build-path dir "blog" "late.md.rkt"))
     (make-parent-directory* post)
     (display-lines-to-file
      (append '("#lang camp/site"
                ""
                "title = \"Timezone Test Site\""
                "url = \"https://test.example.com\""
                "founded = 2024-01-01"
                "authors = [\"Test (test@example.com)\"]")
              (if timezone-line (list timezone-line) '())
              '(""
                "[[collections]]"
                "name = \"blog\""
                "source = \"blog/*\""
                "output-paths = \"blog/[yyyy]/[MM]/[dd]/*/\""))
      site-rkt)
     (display-lines-to-file
      '("#lang punct"
        "---"
        "title: Late"
        "date: 2024-03-05T23:30-08:00"
        "---"
        "Body")
      post)
     (proc (load-site site-rkt)))
   (λ () (delete-directory/files dir))))

(test-case "timezone setting: defaults to #f"
  (call-with-temp-site #f (λ (s) (check-false (site-timezone s)))))

(test-case "timezone setting: rejects unknown zones"
  (check-exn exn:fail?
             (λ () (call-with-temp-site "timezone = \"Mars/Olympus_Mons\"" void))))

(test-case "timezone setting: output paths use the date in the site timezone"
  (define (output-path-of s)
    (path->string (page-output-path (first (site-info-pages (collect s))))))
  (call-with-temp-site
   #f
   (λ (s) (check-equal? (output-path-of s) "blog/2024/03/05/late/index.html")))
  (call-with-temp-site
   "timezone = \"Etc/UTC\""
   (λ (s)
     (check-equal? (site-timezone s) "Etc/UTC")
     (check-equal? (output-path-of s) "blog/2024/03/06/late/index.html"))))
