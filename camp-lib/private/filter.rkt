#lang racket/base

(require racket/format
         racket/list
         racket/string
         gregor
         "dates.rkt"
         "structs.rkt")

(provide filter-pages
         normalize-taxonomy-value)

;; ---------------------------------------------------------------------------
;; Taxonomy Value Normalization

(define (normalize-taxonomy-value val)
  (cond
    [(not val) '()]
    [(string? val)
     (map string-trim (string-split val ","))]
    [(list? val)
     (map (λ (v) (if (symbol? v) (symbol->string v) (~a v))) val)]
    [else '()]))

;; ---------------------------------------------------------------------------
;; Page Filtering

(define (filter-pages pages
                      #:date-from [date-from #f]
                      #:date-to [date-to #f]
                      #:date-key [date-key 'date]
                      #:taxonomies [taxonomies #hasheq()])
  (for/list ([pl (in-list pages)]
             #:when (and (passes-date-filter? pl date-from date-to date-key)
                         (passes-taxonomy-filter? pl taxonomies)))
    pl))

;; ---------------------------------------------------------------------------
;; Date Filtering

(define (passes-date-filter? pl date-from date-to date-key)
  (cond
    [(and (not date-from) (not date-to)) #t]
    [else
     (define metas (page-link-metas pl))
     (define raw-date (hash-ref metas date-key #f))
     (cond
       [(not raw-date) #f]  ; no date means excluded from date-filtered results
       [else
        (define page-date (->date (meta->moment raw-date)))
        (and (or (not date-from) (date>=? page-date (->date date-from)))
             (or (not date-to) (date<=? page-date (->date date-to))))])]))

;; ---------------------------------------------------------------------------
;; Taxonomy Filtering

(define (passes-taxonomy-filter? pl taxonomies)
  (cond
    [(hash-empty? taxonomies) #t]
    [else
     (define metas (page-link-metas pl))
     (for/and ([(tax-key filter-val) (in-hash taxonomies)])
       (define tax-sym (if (symbol? tax-key) tax-key (string->symbol tax-key)))
       (define page-terms (normalize-taxonomy-value (hash-ref metas tax-sym #f)))
       (cond
         [(string? filter-val)
          ;; Exact match: page must have this term
          (member filter-val page-terms)]
         [(list? filter-val)
          ;; Any match: page must have at least one of these terms
          (for/or ([term (in-list filter-val)])
            (member term page-terms))]
         [else #t]))]))  ; ignore invalid filter values
