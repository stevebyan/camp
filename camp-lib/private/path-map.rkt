#lang racket/base

(require gregor
         racket/contract
         racket/format
         racket/list
         racket/path
         racket/string)

(provide source-path-pattern?
         output-path-pattern?
         file-extension?
         non-rkt-file-extension?
         (contract-out
          [format-output-path (-> output-path-pattern?
                                  string?
                                  (or/c date-provider? #f)
                                  (or/c hash? #f)
                                  path?)]
          [pattern-meta-keys (-> string? (listof string?))]))

;; ---------------------------------------------------------------------------
;; Internal helpers

(define *wildcard (string->path "*"))

(define (wildcard? p)
  (equal? *wildcard (if (path? p) p (string->path p))))

(define (contains-brackets? str)
  (regexp-match? #rx"\\[[^][ \t][^][]*\\]" str))

;; ---------------------------------------------------------------------------
;; Contracts

(define source-path-pattern?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (relative-path? v)
             (equal? (if (string? v) (string->path v) v) (simplify-path v))
             (wildcard? (last (explode-path v))))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "Relative path string ending with * and not containing . or .."
             given: "~e") v))]))
   #:name 'source-path-pattern?))

(define output-path-pattern?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(not (relative-path? v))
        (λ (blame)
          (raise-blame-error blame v '(expected: "relative path" given: "~e") v))]
       [(not (equal? (if (string? v) (string->path v) v) (simplify-path v)))
        (λ (blame)
          (raise-blame-error blame v '(expected: "path without . or .." given: "~e") v))]
       [(not (or (for/or ([part (in-list (explode-path v))])
                   (wildcard? part))
                 (contains-brackets? (if (string? v) v (path->string v)))))
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "path containing * element or [bracket] pattern"
             given: "~e") v))]
       [else #t]))
   #:name 'output-path-pattern?))

(define file-extension?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (or (string? v) (bytes? v))
             (regexp-match? #px"^\\.(?!\\.)(?:[^\\\\\\/\r\n.]|\\.[^\\\\\\/\r\n.])+$" v))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "String or bytes starting with '.' and containing no directory separators"
             given: "~e") v))]))
   #:name 'file-extension?))

(define non-rkt-file-extension?
  (flat-contract-with-explanation
   (λ (v)
     (cond
       [(and (file-extension? v)
             (not (equal? v #".rkt"))
             (not (equal? v ".rkt")))]
       [else
        (λ (blame)
          (raise-blame-error
           blame v
           '(expected: "String or bytes starting with '.' (other than '.rkt')"
             given: "~e") v))]))
   #:name 'non-rkt-file-extension?))

;; ---------------------------------------------------------------------------
;; Pattern introspection

(define (pattern-meta-keys pattern)
  (define all-brackets (regexp-match* #rx"\\[([^][]+)\\]" pattern #:match-select cadr))
  (for/list ([b (in-list all-brackets)]
             #:unless (with-handlers ([exn? (λ (_) #f)])
                        (~t (moment 2025 1 15 #:tz 0) b)
                        #t))
    b))

;; ---------------------------------------------------------------------------
;; Path formatting

(define (resolve-bracket pattern metas date-val)
  (define meta-val (and metas (hash-ref metas (string->symbol pattern) #f)))
  (cond
    [meta-val (~a meta-val)]
    [date-val
     (with-handlers ([exn? (λ (_)
                             (error 'format-output-path
                                    "no meta '~a' found and [~a] is not a valid date code"
                                    pattern pattern))])
       (~t date-val pattern))]
    [else
     (error 'format-output-path
            "no meta '~a' found and no date provided to resolve [~a]"
            pattern pattern)]))

(define (resolve-brackets str metas date-val)
  (regexp-replace* #rx"\\[([^][]+)\\]" str
                   (λ (full pattern) (resolve-bracket pattern metas date-val))))

(define (format-output-path pattern slug date-val metas)
  (define p (string->path pattern))
  (define-values (_base _name final-slash?) (split-path p))

  (define parts (explode-path (simplify-path p #f)))
  (define formatted-parts
    (for/list ([part (in-list parts)])
      (define part-str (path->string part))
      (string->path
       (cond
         [(wildcard? part) slug]
         [(contains-brackets? part-str)
          (resolve-brackets part-str metas date-val)]
         [else part-str]))))

  (define new-path (apply build-path formatted-parts))

  (cond
    [final-slash? (build-path new-path "index.html")]
    [else (path-add-extension new-path #".html")]))
