#lang racket/base

;; Feed generation (Atom/RSS) using Splitflap

(require racket/list
         racket/match
         racket/path
         "rerequire.rkt"
         racket/string
         racket/file
         gregor
         splitflap
         "dates.rkt"
         "structs.rkt"
         "main.rkt"
         "log.rkt")

(provide parse-author
         make-feed-tag-uri
         filter-feed-pages
         page-link->feed-item
         generate-feed
         generate-feeds!)

;; ---------------------------------------------------------------------------
;; Author Parsing

(define (parse-author author-str)
  (match (regexp-match #px"^(.+)\\s+\\(([^)]+)\\)$" author-str)
    [(list _ name email)
     (person (string-trim name) (string-trim email))]
    [_ (error 'parse-author "invalid author format: ~a (expected \"Name (email)\")" author-str)]))

;; ---------------------------------------------------------------------------
;; Tag URI Generation

(define (make-feed-tag-uri site-url founded-date collection-names)
  (define domain (url-domain site-url))
  (define year (number->string (->year founded-date)))
  (define specific (normalize-tag-specific (string-join collection-names ",")))
  (mint-tag-uri domain year specific))

;; ---------------------------------------------------------------------------
;; Page Filtering

(define (filter-feed-pages page-links)
  (filter (lambda (pl)
            (define metas (page-link-metas pl))
            (define has-date? (hash-ref metas 'date #f))
            (define is-draft? (hash-ref metas 'draft? #f))
            (and has-date? (not is-draft?)))
          page-links))

;; ---------------------------------------------------------------------------
;; Feed Item Conversion

(define (page-link->feed-item pl feed-tag-uri site-url author render-fn contexts-by-slug)
  (define metas (page-link-metas pl))
  (define slug (hash-ref metas 'slug))
  (define title (page-link-title pl))
  (define relative-url (page-link-url pl))
  (define published (meta->moment (hash-ref metas 'date)))
  (define updated (page-link-updated metas published))

  (define relative-for-join
    (if (string-prefix? relative-url "/")
        (substring relative-url 1)
        relative-url))
  (define absolute-url (url-join site-url relative-for-join))
  (define item-tag-uri (append-specific feed-tag-uri (normalize-tag-specific slug)))
  (define content (get-feed-content slug render-fn contexts-by-slug))

  (feed-item item-tag-uri
             absolute-url
             title
             author
             published
             updated
             content))

(define (page-link-updated metas published)
  (define raw (hash-ref metas 'updated #f))
  (define updated (if raw (meta->moment raw) published))
  (unless (moment>=? updated published)
    (raise-arguments-error 'page-link->feed-item
                           "updated is earlier than date"
                           "page" (hash-ref metas 'slug)
                           "updated" raw
                           "date" (hash-ref metas 'date)))
  updated)

(define (get-feed-content slug render-fn contexts-by-slug)
  (define info (current-site-info))
  (unless info
    (error 'get-feed-content "no site-info available"))
  (define matching-page (hash-ref (site-info-page-by-slug info) slug #f))
  (define ctx (hash-ref contexts-by-slug slug #f))
  (if (and matching-page ctx)
      (render-fn (page-doc matching-page) ctx)
      '(p "Content unavailable")))

;; ---------------------------------------------------------------------------
;; Feed Generation

(define (generate-feed site info feed-cfg contexts-by-slug)
  (define the-site-url (site-url site))
  (define the-site-title (site-title site))
  (define founded (site-founded site))
  (define authors (site-authors site))
  (define root-dir (site-root site))
  (define output-dir (build-path root-dir (site-output-folder site)))

  (define filename (feed-config-filename feed-cfg))
  (define coll-names (feed-config-collections feed-cfg))
  (define render-spec (feed-config-render-with feed-cfg))
  (rerequire! (car render-spec))
  (define render-fn (apply dynamic-require render-spec))

  (define feed-format
    (cond
      [(path-has-extension? (string->path filename) #".atom") 'atom]
      [(path-has-extension? (string->path filename) #".rss") 'rss]
      [else (error 'generate-feed "unknown feed extension: ~a (expected .atom or .rss)" filename)]))

  (define feed-tag-uri (make-feed-tag-uri the-site-url founded coll-names))
  (define author (parse-author (car authors)))

  ;; Each entry's tag URI derives from its own collection, so entry IDs stay
  ;; stable when collections are added to or removed from a feed.
  (define feed-items
    (append*
     (for/list ([coll-name (in-list coll-names)])
       (define coll-tag-uri (make-feed-tag-uri the-site-url founded (list coll-name)))
       (for/list ([pl (in-list (filter-feed-pages (get-collection coll-name)))])
         (page-link->feed-item pl coll-tag-uri the-site-url author render-fn contexts-by-slug)))))

  (define the-feed (feed feed-tag-uri the-site-url the-site-title feed-items))
  (define feed-url (url-join the-site-url filename))

  (express-xml the-feed feed-format feed-url))


;; ---------------------------------------------------------------------------
;; Build Integration

(define (generate-feeds! site info contexts-by-slug)
  (define feeds (site-feeds site))
  (when (and feeds (not (null? feeds)))
    (define root-dir (site-root site))
    (define output-dir (build-path root-dir (site-output-folder site)))

    (for ([feed-cfg (in-list feeds)])
      (define filename (feed-config-filename feed-cfg))
      (define output-path (build-path output-dir filename))
      (define xml-str (generate-feed site info feed-cfg contexts-by-slug))

      (make-parent-directory* output-path)
      (call-with-output-file output-path
        (lambda (out) (display xml-str out))
        #:exists 'replace)

      (log-camp-debug "generated feed: ~a" filename))))
