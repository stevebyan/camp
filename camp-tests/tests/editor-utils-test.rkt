#lang racket/base

;; Tests for the in-app editor's pure helpers

(require racket/list
         racket/string
         rackunit
         rackunit/text-ui
         camp/app/private/editor-utils)

(define editor-utils-tests
  (test-suite
   "Editor helpers"

   (test-case "title shows filename and enclosing folder"
     (check-equal? (editor-title (string->path "/sites/camp-demo/site.rkt") #f)
                   "site.rkt — camp-demo")
     (check-equal? (editor-title (string->path "/sites/camp-demo/blog/first-post.md.rkt") #f)
                   "first-post.md.rkt — blog"))

   (test-case "dirty title gains a bullet"
     (check-equal? (editor-title (string->path "/sites/camp-demo/site.rkt") #t)
                   "• site.rkt — camp-demo"))

   (test-case "file at filesystem root has no folder suffix"
     (check-equal? (editor-title (string->path "/site.rkt") #f) "site.rkt"))

   (test-case "a markup attribute becomes the colorer's color category"
     (check-equal? (promote-markup (hasheq 'type 'text 'markup 'emphasis))
                   (hasheq 'type 'text 'markup 'emphasis 'color 'markup-emphasis))
     (define plain (hasheq 'type 'text 'comment? #t))
     (check-eq? (promote-markup plain) plain)
     (check-eq? (promote-markup 'symbol) 'symbol))

   (test-case "markup-aware wraps a lexer and passes everything else through"
     (define (fake in offset mode)
       (values "x" (hasheq 'type 'keyword 'markup 'heading) '|(| 1 2 0 (list offset mode)))
     (define-values (lexeme attribs paren start end backup mode)
       ((markup-aware fake) (open-input-string "") 7 'm))
     (check-equal? (hash-ref attribs 'color) 'markup-heading)
     (check-equal? (list lexeme paren start end backup mode) '("x" |(| 1 2 0 (7 m))))

   (test-case "saved-message follows vim's write report plus a timestamp"
     (check-equal? (saved-message "one\ntwo\nthree\n" 246 "14:22 Aug 22")
                   "Saved: 3L, 246B written • 14:22 Aug 22"))

   (test-case "saved-message counts a final line without a trailing newline"
     (check-equal? (saved-message "a\nb" 3 "09:05 Jan 1")
                   "Saved: 2L, 3B written • 09:05 Jan 1")
     (check-equal? (saved-message "" 0 "09:05 Jan 1")
                   "Saved: 0L, 0B written • 09:05 Jan 1"))

   (test-case "internal editor used only when no external editor is set"
     (check-true (use-internal-editor? ""))
     (check-false (use-internal-editor? "/usr/bin/vim"))
     (check-false (use-internal-editor? "/Applications/BBEdit.app")))

   (test-case "fill-paragraph wraps greedily at the given width"
     (check-equal? (fill-paragraph "aaa bbb ccc ddd" 7) "aaa bbb\nccc ddd")
     (check-equal? (fill-paragraph "aaa bbb ccc" 80) "aaa bbb ccc"))

   (test-case "fill-paragraph rejoins hard-wrapped lines and collapses spaces"
     (check-equal? (fill-paragraph "aaa\nbbb    ccc\n\tddd" 80) "aaa bbb ccc ddd"))

   (test-case "fill-paragraph carries the first line's indent to wrapped lines"
     (check-equal? (fill-paragraph "  aaa bbb ccc" 6) "  aaa\n  bbb\n  ccc"))

   (test-case "fill-paragraph never breaks a word longer than the width"
     (check-equal? (fill-paragraph "aa indivisible bb" 5) "aa\nindivisible\nbb"))

   (test-case "fill-paragraph leaves blank text alone"
     (check-equal? (fill-paragraph "" 10) "")
     (check-equal? (fill-paragraph "   " 10) "   "))

   (test-case "fill-unit wraps a bullet item with hanging indent"
     (check-equal? (fill-unit (vector "- aaa bbb ccc ddd") 0 9)
                   (list 0 0 "- aaa bbb\n  ccc ddd")))

   (test-case "fill-unit hangs ordered-list markers by their width"
     (check-equal? (fill-unit (vector "12. aaa bbb ccc") 0 11)
                   (list 0 0 "12. aaa bbb\n    ccc"))
     (check-equal? (fill-unit (vector "1) aaa bbb") 0 6)
                   (list 0 0 "1) aaa\n   bbb")))

   (test-case "fill-unit never merges adjacent list items"
     (check-equal? (fill-unit (vector "- aaa bbb ccc" "- second") 0 7)
                   (list 0 0 "- aaa\n  bbb\n  ccc"))
     (check-equal? (fill-unit (vector "- one" "- two aaa bbb" "- three") 1 7)
                   (list 1 1 "- two\n  aaa\n  bbb")))

   (test-case "fill-unit treats a wrapped number other than 1 as paragraph text"
     (define para (vector "The right wing didn't care before" "2018. Then my district elected"))
     (check-equal? (fill-unit para 1 80)
                   (list 0 1 "The right wing didn't care before 2018. Then my district elected"))
     (check-equal? (fill-unit (vector "aaa bbb" "2018. ccc ddd") 1 9)
                   (list 0 1 "aaa bbb\n2018. ccc\nddd"))
     (check-equal? (auto-filled '("aaa" "2018. ccc ddd") 1 9) "2018. ccc\nddd"))

   (test-case "fill-unit still splits at markers that can interrupt a paragraph"
     (check-equal? (fill-unit (vector "aaa" "1. bbb") 1 80) (list 1 1 "1. bbb"))
     (check-equal? (fill-unit (vector "aaa" "- bbb") 1 80) (list 1 1 "- bbb"))
     (check-equal? (fill-unit (vector "1. aaa" "2. bbb ccc") 1 7) (list 1 1 "2. bbb\n   ccc"))
     (check-equal? (fill-unit (vector "aaa" "" "7. bbb ccc") 2 7) (list 2 2 "7. bbb\n   ccc")))

   (test-case "fill-unit from a continuation line fills the whole owning item"
     (check-equal? (fill-unit (vector "- aaa bbb" "lazy ccc") 1 20)
                   (list 0 1 "- aaa bbb lazy ccc")))

   (test-case "fill-unit keeps a nested item's indent"
     (check-equal? (fill-unit (vector "  - aaa bbb ccc") 0 9)
                   (list 0 0 "  - aaa\n    bbb\n    ccc")))

   (test-case "fill-unit keeps blockquote prefixes on every line"
     (check-equal? (fill-unit (vector "> aaa bbb ccc") 0 7)
                   (list 0 0 "> aaa\n> bbb\n> ccc"))
     (check-equal? (fill-unit (vector "> - aaa bbb") 0 7)
                   (list 0 0 "> - aaa\n>   bbb")))

   (test-case "boundaries hold inside a blockquote"
     (define quoted (vector "> aaa bbb" ">" "> ccc" "> # ddd" "> eee"))
     (check-equal? (fill-unit quoted 0 20) (list 0 0 "> aaa bbb"))
     (check-equal? (fill-unit quoted 2 20) (list 2 2 "> ccc"))
     (check-equal? (fill-unit quoted 4 20) (list 4 4 "> eee"))
     (check-false (fill-unit quoted 1 20))
     (check-false (fill-unit quoted 3 20))
     (check-equal? (auto-filled '("> aaa bbb ccc" ">" "> ddd") 0 9) "> aaa bbb\n> ccc\n>\n> ddd"))

   (test-case "fill-unit on a plain paragraph matches fill-paragraph behavior"
     (check-equal? (fill-unit (vector "aaa bbb ccc ddd") 0 7)
                   (list 0 0 "aaa bbb\nccc ddd"))
     (check-equal? (fill-unit (vector "intro" "" "aaa bbb ccc") 2 80)
                   (list 2 2 "aaa bbb ccc")))

   (test-case "fill-unit refuses structurally unsafe lines"
     (check-false (fill-unit (vector "aaa" "" "bbb") 1 80))
     (check-false (fill-unit (vector "# Heading") 0 80))
     (check-false (fill-unit (vector "```racket") 0 80))
     (check-false (fill-unit (vector "```" "(code aaa bbb ccc ddd)" "```") 1 10))
     (check-false (fill-unit (vector "---") 0 80))
     (check-false (fill-unit (vector "===") 0 80))
     (check-false (fill-unit (vector "| a | b |") 0 80))
     (check-false (fill-unit (vector "#lang punct") 0 80)))

   (test-case "fill-unit refuses the top metadata block but not the body"
     (define doc (vector "#lang punct" "" "---" "title: aaa bbb ccc ddd" "---" "" "body text"))
     (check-false (fill-unit doc 3 10))
     (check-equal? (fill-unit doc 6 80) (list 6 6 "body text")))

   (test-case "auto-fill-edits breaks an overlong line where fill-unit would"
     (for ([l (in-list '("aaa bbb ccc ddd" "- aaa bbb ccc ddd" "12. aaa bbb ccc"
                         "  aaa bbb ccc" "> aaa bbb ccc" "> - aaa bbb ccc"
                         "aa indivisible bb"))])
       (check-equal? (apply-edits l (auto-fill-edits (vector l) 0 9))
                     (third (fill-unit (vector l) 0 9))
                     l)))

   (test-case "auto-fill-edits lists whitespace runs rightmost first"
     (check-equal? (auto-fill-edits (vector "> aaa bbb  ccc") 0 5)
                   '((9 11 "\n> ") (5 6 "\n> "))))

   (test-case "auto-fill-edits leaves lines that fit, and trailing space, alone"
     (check-equal? (auto-fill-edits (vector "aaa bbb") 0 7) '())
     (check-equal? (auto-fill-edits (vector "aaa bbb ") 0 7) '())
     (check-equal? (auto-fill-edits (vector "indivisible") 0 5) '()))

   (test-case "auto-fill-edits carries overflow onto the next line of the unit"
     (check-equal? (auto-fill-edits (vector "aaa bbb ccc" "ddd") 0 7)
                   '((11 12 " ") (7 8 "\n")))
     (check-equal? (auto-filled '("aaa bbb ccc" "ddd eee") 0 7) "aaa bbb\nccc ddd\neee")
     (check-equal? (auto-filled '("> aaa bbb ccc" "> ddd") 0 9) "> aaa bbb\n> ccc ddd")
     (check-equal? (auto-filled '("- aaa bbb ccc" "  ddd") 0 9) "- aaa bbb\n  ccc ddd")
     (check-equal? (auto-filled '("- aaa" "  bbb ccc ddd" "  eee") 1 9) "  bbb ccc\n  ddd eee"))

   (test-case "auto-fill-edits stops carrying at the first line that fits"
     (check-equal? (auto-filled '("aaa bbb ccc" "dd" "eee fff") 0 7) "aaa bbb\nccc dd\neee fff")
     (check-equal? (auto-filled '("aaa bbb ccc" "ddddd" "ee") 0 7) "aaa bbb\nccc\nddddd\nee"))

   (test-case "auto-fill-edits carries nothing past the unit or a hard break"
     (for ([next (in-list '("" "- ddd" "# ddd" "> ddd"))])
       (check-equal? (auto-filled (list "aaa bbb ccc" next) 0 7)
                     (string-append "aaa bbb\nccc\n" next)))
     (check-equal? (auto-filled '("aaa bbb ccc  " "ddd") 0 7) "aaa bbb\nccc  \nddd")
     (check-equal? (auto-filled '("aaa bbb ccc\\" "ddd") 0 7) "aaa bbb\nccc\\\nddd"))

   (test-case "position-after-edits keeps a caret at an edit's start in place"
     (define edits '((11 12 " ") (7 8 "\n  ")))
     (check-equal? (position-after-edits 7 edits) 7)
     (check-equal? (position-after-edits 11 edits) 13)
     (check-equal? (position-after-edits 12 edits) 14)
     (check-equal? (position-after-edits 3 edits) 3))

   (test-case "auto-fill-edits refuses what fill-unit refuses"
     (check-equal? (auto-fill-edits (vector "# Heading aaa bbb ccc") 0 10) '())
     (check-equal? (auto-fill-edits (vector "```" "(code aaa bbb ccc ddd)" "```") 1 10) '())
     (check-equal? (auto-fill-edits (vector "---" "title: aaa bbb ccc" "---") 1 10) '()))))

(define (auto-filled ls idx width)
  (apply-edits (string-join (list-tail ls idx) "\n")
               (auto-fill-edits (list->vector ls) idx width)))

(define (apply-edits l edits)
  (for/fold ([l l]) ([e (in-list edits)])
    (string-append (substring l 0 (first e)) (third e) (substring l (second e)))))

(module+ main
  (run-tests editor-utils-tests))

(module+ test
  (run-tests editor-utils-tests))
