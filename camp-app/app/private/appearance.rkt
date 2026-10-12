#lang racket/base

;; Appearance: applies the user's light/dark scheme selections and mode to
;; framework's color machinery (design §3). Camp never registers schemes —
;; it fills both polarity slots of every entry's adjustment table from the
;; selected schemes, so polarity flips (including macOS appearance changes
;; in System mode) are handled entirely by framework.

(require camp/app/private/framework-prefs
         framework
         mrlib/panel-wob
         racket/class
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         camp/app/private/settings
         camp/app/private/theme)

(provide init-appearance!
         apply-scheme-tables!
         apply-mode!
         set-color-scheme-slot!
         value-spec->object
         @canvas-bg
         @vim-selection-color)

;; The vim selection color rides the same scheme machinery as everything else;
;; the defaults match the vim tool's hardcoded color. (The caret is a fixed
;; color, see editor.rkt.)
(color-prefs:add-color-scheme-entry 'camp:vim-selection-color
                                    "lightsteelblue" (make-color 70 90 120))

;; Punct's Markdown categories, named like framework's own token entries so
;; the colorer's style lookup finds them (design §4). Built-in defaults echo
;; the standard type each such token also carries: (category light dark
;; bold? italic?)
(define blue (make-color 38 38 128))
(define pale-blue (make-color 157 157 250))
(define rust (make-color 151 69 43))
(for ([e (in-list `((markup-heading ,blue ,pale-blue #t #f)
                    (markup-emphasis "black" "white" #f #t)
                    (markup-strong "black" "white" #t #f)
                    (markup-code ,(make-color 41 128 38) ,(make-color 140 212 140) #f #f)
                    (markup-link ,blue ,pale-blue #f #f)
                    (markup-list "brown" ,rust #f #f)
                    (markup-quote "brown" ,rust #f #f)
                    (markup-rule "brown" ,rust #f #f)
                    (markup-meta "brown" ,rust #f #f)))])
  (color-prefs:add-color-scheme-entry (racket:short-sym->pref-name (first e))
                                      (second e) (third e)
                                      #:style (racket:short-sym->style-name (first e))
                                      #:bold? (fourth e) #:italic? (fifth e)))

;; ============================================================================
;; Plan values → framework objects

(define (hex->color hex)
  (make-color (string->number (substring hex 1 3) 16)
              (string->number (substring hex 3 5) 16)
              (string->number (substring hex 5 7) 16)))

;; A plan value-spec (design §3): bare hex → color%; (hex bold? italic?
;; underline?) → style-delta%
(define (value-spec->object spec)
  (match spec
    [(? string?) (hex->color spec)]
    [(list hex bold? italic? underline?)
     (define d (make-object style-delta%))
     (send d set-delta-foreground (hex->color hex))
     (when bold? (send d set-weight-on 'bold))
     (when italic? (send d set-style-on 'italic))
     (when underline? (send d set-underlined-on #t))
     d]))

;; ============================================================================
;; Adjustment tables

;; Rewrites one polarity slot of an entry's adjustment table — a pref
;; holding (hash scheme-name → color%/style-delta%). The naming and hash
;; shape are framework's prefs file format, pinned against the public API
;; by appearance-test.rkt. #f removes the slot so built-ins show through.
(define (set-color-scheme-slot! entry scheme-sym value)
  (define pref (string->symbol (format "color-scheme-entry:~a" entry)))
  (define table (preferences:get pref))
  (preferences:set pref (if value
                            (hash-set table scheme-sym value)
                            (hash-remove table scheme-sym))))

;; A built-in scheme keeps framework's colors except for prose: DrRacket
;; paints the text category in its string color, which suits Scribble but not
;; a Punct page, so text takes the scheme's default text color instead —
;; framework's own defaults for framework:default-text-color
(define (built-in-plan dark?)
  (list (list 'framework:syntax-color:scheme:text (if dark? 'dark 'light)
              (list (if dark? "#ffffff" "#000000") #f #f #f))))

(define (selection-plan selection schemes dark?)
  (cond
    [(findf (λ (s) (equal? (scheme-name s) selection)) schemes)
     => scheme-entry-plan]
    [else (built-in-plan dark?)]))

;; The 'classic slots carry the light selection, 'white-on-black the dark
;; one; each preferences:set fires framework's per-entry change callback
(define (apply-scheme-tables!)
  (define schemes (obs-peek @color-schemes))
  (define light-plan (selection-plan (obs-peek @light-scheme) schemes #f))
  (define dark-plan (selection-plan (obs-peek @dark-scheme) schemes #t))
  (define (plan-value plan entry)
    (cond [(assq entry plan) => (λ (p) (value-spec->object (third p)))]
          [else #f]))
  (for ([entry (in-list scheme-entry-names)])
    (set-color-scheme-slot! entry 'classic (plan-value light-plan entry))
    (set-color-scheme-slot! entry 'white-on-black (plan-value dark-plan entry))))

;; ============================================================================
;; Mode

;; The app's own appearance, which the 'platform case reads back, is set by
;; app-appearance.rkt
(define (apply-mode!)
  (match (obs-peek @appearance-mode)
    ['light (preferences:set 'framework:white-on-black-mode? #f)
            (color-prefs:set-current-color-scheme 'classic)]
    ['dark (preferences:set 'framework:white-on-black-mode? #t)
           (color-prefs:set-current-color-scheme 'white-on-black)]
    [_ (preferences:set 'framework:white-on-black-mode? 'platform)
       (color-prefs:set-current-color-scheme
        (if (white-on-black-panel-scheme?) 'white-on-black 'classic))]))

;; ============================================================================
;; Editor-facing observables

(define (entry-observable entry)
  (define/obs @color (color-prefs:lookup-in-color-scheme entry))
  (color-prefs:register-color-scheme-entry-change-callback
   entry (λ (c) (@color . := . c)))
  @color)

(define @canvas-bg (entry-observable 'framework:basic-canvas-background))
(define @vim-selection-color (entry-observable 'camp:vim-selection-color))

;; ============================================================================
;; Startup

(define (init-appearance!)
  (apply-scheme-tables!)
  (apply-mode!)
  (obs-observe! @appearance-mode (λ (_) (apply-mode!)))
  (for ([@o (in-list (list @color-schemes @light-scheme @dark-scheme))])
    (obs-observe! @o (λ (_) (apply-scheme-tables!) (apply-mode!)))))
