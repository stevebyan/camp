#lang racket/base

;; Tests for camp/private/path-map module

(require rackunit
         racket/path
         racket/hash
         gregor
         (only-in camp/private/path-map
                  source-path-pattern?
                  output-path-pattern?
                  file-extension?
                  non-rkt-file-extension?
                  format-output-path
                  pattern-meta-keys))

;; ---------------------------------------------------------------------------
;; source-path-pattern? tests

;; Valid patterns
(check-true (source-path-pattern? "blog/*"))
(check-true (source-path-pattern? "blog/*/*"))
(check-true (source-path-pattern? "blog/subfolder/*"))
(check-true (source-path-pattern? "*"))
(check-true (source-path-pattern? (build-path "blog" "*")))

;; Invalid patterns
(check-false (source-path-pattern? ""))
(check-false (source-path-pattern? "../blog/*"))     ; cannot start with 'up
(check-false (source-path-pattern? "./blog/*"))      ; cannot start with 'same
(check-false (source-path-pattern? "/blog/*"))       ; cannot be absolute path
(check-false (source-path-pattern? "foo/../blog/*")) ; cannot contain 'up
(check-false (source-path-pattern? "foo/./blog/*"))  ; cannot contain 'same
(check-false (source-path-pattern? "blog/*/other"))  ; last element must be *
(check-false (source-path-pattern? "blog/**"))       ; last element must be exactly *

;; ---------------------------------------------------------------------------
;; output-path-pattern? tests

;; Valid patterns
(check-true (output-path-pattern? "article/*/[MM]"))
(check-true (output-path-pattern? "article/*"))
(check-true (output-path-pattern? "article/*/"))
(check-true (output-path-pattern? "article/*/*"))
(check-true (output-path-pattern? "*"))
(check-true (output-path-pattern? "blog/[yyyy]/[MM]/*/"))
(check-true (output-path-pattern? "archive/[MMM]/*/"))      ; abbreviated month
(check-true (output-path-pattern? "[yyyy]/[MM]/[dd]/*"))

;; Invalid patterns
(check-false (output-path-pattern? ""))
(check-false (output-path-pattern? "./*"))            ; cannot start with same
(check-false (output-path-pattern? "../*"))           ; cannot start with up
(check-false (output-path-pattern? "foo/../*"))       ; cannot contain ..
(check-false (output-path-pattern? "foo/./*"))        ; cannot contain .
(check-false (output-path-pattern? "/*"))             ; cannot be absolute
(check-false (output-path-pattern? "foo/**"))         ; must contain a single * as one element
(check-false (output-path-pattern? "foo/bar"))        ; no * or brackets
(check-false (output-path-pattern? "foo"))            ; no * or brackets

;; Brackets alone satisfy the dynamic element requirement
(check-true (output-path-pattern? "newsletter/[issue]/"))
(check-true (output-path-pattern? "[volume]/[issue]/"))
(check-true (output-path-pattern? "blog/[yyyy]/[category]/*/"))

;; Any non-empty bracket content is valid (resolved at format time)
(check-true (output-path-pattern? "blog/[invalid]/*/"))
(check-true (output-path-pattern? "blog/[zzz]/*/"))
(check-true (output-path-pattern? "blog/[hello-world]/*/"))
(check-true (output-path-pattern? "foo/[UPPER]/*/"))
(check-true (output-path-pattern? "foo/[anything-goes]/"))

;; Empty or whitespace-only brackets don't count
(check-false (output-path-pattern? "foo/[]/"))
(check-false (output-path-pattern? "foo/[ ]/"))

;; ---------------------------------------------------------------------------
;; file-extension? tests

;; Valid extensions
(check-true (file-extension? ".txt"))
(check-true (file-extension? ".file.txt"))
(check-true (file-extension? #".txt"))
(check-true (file-extension? #".file.txt"))
(check-true (file-extension? ".md.rkt"))

;; Invalid extensions
(check-false (file-extension? "txt"))           ; no period
(check-false (file-extension? "txt.page"))      ; must start with period
(check-false (file-extension? ".txt..page"))    ; no double periods
(check-false (file-extension? "."))             ; "same" path element doesn't count
(check-false (file-extension? ".."))            ; "up" path element doesn't count
(check-false (file-extension? "..txt"))         ; no double periods
(check-false (file-extension? ".txt/.page"))    ; no / allowed
(check-false (file-extension? ".txt\\.page"))   ; no \ allowed
(check-false (file-extension? ".txt\n.page"))   ; no newline allowed
(check-false (file-extension? ".txt.page\r"))   ; no carriage return allowed

;; Bytes versions
(check-false (file-extension? #"txt"))          ; no period (bytes)
(check-false (file-extension? #"txt.page"))     ; must start with period (bytes)
(check-false (file-extension? #".txt..page"))   ; no double periods (bytes)
(check-false (file-extension? #"."))            ; "same" path element doesn't count (bytes)
(check-false (file-extension? #".."))           ; "up" path element doesn't count (bytes)
(check-false (file-extension? #"..txt"))        ; no double periods (bytes)
(check-false (file-extension? #".txt/.page"))   ; no / allowed (bytes)
(check-false (file-extension? #".txt\\.page"))  ; no \ allowed (bytes)
(check-false (file-extension? #".txt\n.page"))  ; no newline allowed (bytes)
(check-false (file-extension? #".txt.page\r"))  ; no carriage return allowed (bytes)

;; Non-string/bytes
(check-false (file-extension? 'non-string-or-bytes))

;; ---------------------------------------------------------------------------
;; non-rkt-file-extension? tests

(check-true (non-rkt-file-extension? ".md.rkt"))
(check-true (non-rkt-file-extension? ".txt"))
(check-true (non-rkt-file-extension? ".page.rkt"))

(check-false (non-rkt-file-extension? ".rkt"))
(check-false (non-rkt-file-extension? #".rkt"))

;; ---------------------------------------------------------------------------
;; format-output-path tests

(define test-date (date 2025 1 15))

;; Basic slug substitution (no dates, no metas)
(check-equal? (format-output-path "*" "my-post" #f #f)
              (string->path "my-post.html"))
(check-equal? (format-output-path "posts/*" "my-post" #f #f)
              (string->path "posts/my-post.html"))

;; Trailing slash -> index.html
(check-equal? (format-output-path "*/" "my-post" #f #f)
              (string->path "my-post/index.html"))
(check-equal? (format-output-path "posts/*/" "my-post" #f #f)
              (string->path "posts/my-post/index.html"))

;; Date patterns with bracketed syntax
(check-equal? (format-output-path "blog/[yyyy]/[MM]/*/" "my-post" test-date #f)
              (string->path "blog/2025/01/my-post/index.html"))
(check-equal? (format-output-path "[yyyy]/[MM]/[dd]/*" "my-post" test-date #f)
              (string->path "2025/01/15/my-post.html"))

;; Single-digit date formats
(check-equal? (format-output-path "blog/[y]/[M]/[d]/*" "my-post" (date 2025 3 5) #f)
              (string->path "blog/2025/3/5/my-post.html"))

;; Abbreviated month name
(check-equal? (format-output-path "archive/[MMM]/*" "my-post" test-date #f)
              (string->path "archive/Jan/my-post.html"))

;; Complex pattern with trailing slash
(check-equal? (format-output-path "archive/[yyyy]-[MM]/*/" "my-post" test-date #f)
              (string->path "archive/2025-01/my-post/index.html"))

;; Error when date patterns present but no date provided
(check-exn exn:fail?
           (lambda () (format-output-path "blog/[yyyy]/*" "my-post" #f #f))
           "Should error when date codes present but no date given")

(check-exn exn:fail?
           (lambda () (format-output-path "[MM]/[dd]/*/" "my-post" #f #f))
           "Should error when multiple date codes present but no date given")

;; ---------------------------------------------------------------------------
;; Meta interpolation in format-output-path

;; Meta value substitution
(check-equal? (format-output-path "newsletter/[issue]/*/" "my-article" #f
                                  (hasheq 'issue 42))
              (string->path "newsletter/42/my-article/index.html"))

;; Mixed date + meta
(check-equal? (format-output-path "blog/[yyyy]/[category]/*/" "post" test-date
                                  (hasheq 'category "tech"))
              (string->path "blog/2025/tech/post/index.html"))

;; Pattern without slug — only meta bracket
(check-equal? (format-output-path "newsletter/[issue]/" "unused" #f
                                  (hasheq 'issue 42))
              (string->path "newsletter/42/index.html"))

;; Multiple meta keys
(check-equal? (format-output-path "[volume]/[issue]/" "unused" #f
                                  (hasheq 'volume 3 'issue 7))
              (string->path "3/7/index.html"))

;; Metas-first: meta key shadows a CLDR-valid pattern
(check-equal? (format-output-path "[age]/*" "x" test-date
                                  (hasheq 'age "old"))
              (string->path "old/x.html"))

;; Error: bracket not in metas and not valid CLDR
(check-exn exn:fail?
           (lambda () (format-output-path "[issue]/*" "x" #f (hasheq)))
           "Should error when meta key not found and no date")

;; Error: metas is #f and bracket not valid CLDR
(check-exn exn:fail?
           (lambda () (format-output-path "[issue]/*" "x" #f #f))
           "Should error when no metas provided")

;; ---------------------------------------------------------------------------
;; pattern-meta-keys tests

(check-equal? (pattern-meta-keys "newsletter/[issue]/*/") '("issue"))
(check-equal? (pattern-meta-keys "[volume]/[issue]/") '("volume" "issue"))
(check-equal? (pattern-meta-keys "blog/[yyyy]/[MM]/*/") '())
(check-equal? (pattern-meta-keys "log/[yyyy]/[MM]/[dd]/[HH][mm]/*/") '())
(check-equal? (pattern-meta-keys "blog/[yyyy]/[category]/*/") '("category"))
(check-equal? (pattern-meta-keys "posts/*/") '())
(check-equal? (pattern-meta-keys "*") '())
