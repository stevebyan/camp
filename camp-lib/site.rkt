#lang racket/base

(module reader toml/config/custom
  (require splitflap/constructs
           gregor
           racket/match
           (only-in tzinfo tzid-exists?)
           (only-in camp/private/path-map
                    source-path-pattern?
                    output-path-pattern?
                    non-rkt-file-extension?))
  (provide author-string? feed-filename? render-spec? timezone-name?)

  (define (author-string? s) ; "Name (email)" format
    (match s
      [(regexp #px".+\\s+\\((.+)\\)\\s*$" (list _ email))
       (email-address? email)]
      [_ #f]))

  (define (feed-filename? v)
    (and (string? v) (regexp-match? #px"\\.(?:atom|rss)$" v)))

  (define (timezone-name? v)
    (and (string? v) (tzid-exists? v)))

  ; This validator does it all in one shot rather than daisy-chaining with
  ; readable-datum? in the schema, so we can document it more easily
  (define (render-spec? v__)
    (let* ([v_ (readable-datum? v__)]
           [v (and (box? v_) (unbox v_))])
      (and (list? v)
           (= 2 (length v))
           (module-path? (car v))
           (symbol? (cadr v))
           v)))

  #:schema
  ([title non-empty-string? required]
   [url valid-url-string? required]
   [founded date-provider? required]
   [authors (listof author-string?) required]
   [timezone timezone-name? (optional #f)]
   [sources non-rkt-file-extension? (optional ".md.rkt")]
   [static-folder path-string? (optional "static")]
   [output-folder path-string? (optional "publish")]
   [deploy-script path-string? (optional #f)]
   [default-render render-spec? (optional #f)]
   [collections
    (array-of table
              [name non-empty-string? required]
              [source source-path-pattern? required]
              [output-paths output-path-pattern? required]
              [render-with render-spec? (optional #f)]
              [order (or/c "ascending" "descending") (optional "descending")]
              [sort-key non-empty-string? (optional "date")]
              [taxonomies (listof non-empty-string?) (optional '())])
    required]
   [feeds
    (array-of table
              [filename feed-filename? required]
              [collections (listof non-empty-string?) required]
              [render-with render-spec? required])
    optional]))

(require 'reader)
(provide author-string? feed-filename? render-spec? timezone-name?)
