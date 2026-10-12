#lang racket/base

;; In-app editor: one syntax-highlighted window per source file.
;;
;; racket:text% colors any #lang whose reader supplies a 'color-lexer (punct,
;; camp/site, camp/book, racket…) by dispatching off the buffer's #lang line,
;; so no per-filetype configuration is needed here; punct's Markdown structure
;; arrives as a 'markup token attribute (see editor-utils).

(require camp/app/private/framework-prefs ; must precede framework (see that module)
         framework
         (only-in gregor now ~t)
         racket/class
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/path
         racket/string
         ;; must be a static require: parent-frame is a local member name,
         ;; which set-field! needs at compile time
         (only-in drracket-vim-tool/private/text vim-emulation-mixin parent-frame)
         (only-in camp/app/private/appearance @canvas-bg @vim-selection-color)
         camp/app/private/complete
         camp/app/private/editor-utils
         camp/app/private/fonts
         camp/app/private/gui
         (only-in camp/app/private/settings
                  @vim-mode @fill-column @auto-fill? @line-numbers? @editor-geometry))

(provide open-editor! close-all-editors!)

;; ============================================================================
;; Text class

(define camp-editor-keymap
  (let ([km (new keymap:aug-keymap%)])
    (send km add-function "camp:save"
          (λ (ed _evt) (send ed save-file #f 'text)))
    (send km add-function "camp:close-window"
          (λ (ed _evt) (send (send ed get-top-level-window) close-editor-window!)))
    (send km add-function "camp:find"
          (λ (ed _evt) (send (send ed get-top-level-window) focus-find-field!)))
    (send km add-function "camp:fill-paragraph"
          (λ (ed _evt) (send ed fill-paragraph!)))
    (send km add-function "camp:cycle-font"
          (λ (_ed _evt) (cycle-font-slot!)))
    (send km map-function "d:s" "camp:save")
    (send km map-function "c:s" "camp:save")
    (send km map-function "d:w" "camp:close-window")
    (send km map-function "d:f" "camp:find")
    (send km map-function "d:j" "camp:fill-paragraph")
    (send km map-function "d:s:f" "camp:cycle-font")
    km))

(define completion-idle-ms 2000)

(define (camp-editor-mixin %)
  (class %
    (init-field @dirty? on-save-cb)
    (inherit get-filename get-text compute-racket-amount-to-indent
             set-max-undo-history)
    (super-new)
    ;; text% records no undo history by default
    (set-max-undo-history 'forever)

    ;; Route punct's 'markup tokens to the markup-* color entries
    (define/override (start-colorer token-sym->style get-token pairs)
      (super start-colorer token-sym->style (markup-aware get-token) pairs))

    (define/public (refresh-completions)
      (define fn (get-filename))
      (when fn (request-completions! fn (get-text))))
    (define completion-timer
      (new timer% [notify-callback (λ () (refresh-completions))]))

    ;; Langs like punct publish an indenter via get-info; framework auto-wires
    ;; only the color lexer, so the indenter is looked up here
    (define lang-indenter #f)
    (define/public (refresh-lang-info!)
      (define info
        (with-handlers ([(λ (_) #t) (λ (_) #f)])
          (read-language (open-input-string (get-text)) (λ () #f))))
      (set! lang-indenter (and info (info 'drracket:indentation #f))))
    (define/augment (compute-amount-to-indent pos)
      (or (and lang-indenter (lang-indenter this pos))
          (compute-racket-amount-to-indent pos)))

    (define/override (set-modified mod?)
      (@dirty? . := . (and mod? #t))
      (super set-modified mod?))
    (define/augment (after-save-file success?)
      (when success?
        (refresh-completions)
        (refresh-lang-info!)
        (on-save-cb (get-filename)))
      (inner (void) after-save-file success?))
    (define/augment (after-insert start len)
      (send completion-timer start completion-idle-ms #t)
      (inner (void) after-insert start len))
    (define/augment (after-delete start len)
      (send completion-timer start completion-idle-ms #t)
      (inner (void) after-delete start len))
    (define/augment (on-close)
      (send completion-timer stop)
      (inner (void) on-close))
    (define/override (get-all-words)
      (define fn (get-filename))
      (remove-duplicates
       (append (if fn (completions-for fn) '())
               (buffer-words (get-text)))))
    (define/override (get-keymaps)
      (cons camp-editor-keymap (super get-keymaps)))

    ;; Hard-wrap the markdown fill unit (list item, quoted or plain
    ;; paragraph) around the caret
    (inherit get-start-position position-paragraph get-top-level-window
             paragraph-start-position paragraph-end-position
             delete insert set-position begin-edit-sequence end-edit-sequence)
    (define/private (status! msg)
      (send (get-top-level-window) set-vim-status-message msg))
    (define/private (buffer-lines)
      (list->vector (regexp-split #rx"\n" (get-text))))
    (define/public (fill-paragraph!)
      (define result (fill-unit (buffer-lines)
                                (position-paragraph (get-start-position))
                                (obs-peek @fill-column)))
      (cond
        [(not result)
         (status! "No wrappable paragraph here")]
        [else
         (define start (paragraph-start-position (first result)))
         (define end (paragraph-end-position (second result)))
         (define filled (third result))
         (cond
           [(equal? filled (get-text start end))
            (status! (format "Already wrapped at ~a columns" (obs-peek @fill-column)))]
           [else
            (status! "")
            (begin-edit-sequence)
            (delete start end)
            (insert filled start)
            (end-edit-sequence)])
         (set-position (+ start (string-length filled)))]))

    ;; Auto-fill: text% soft-wraps by pixel width, so only a hard break as a
    ;; typed character overflows the column agrees with fill-paragraph! An
    ;; insert at the caret would push the caret past it, so the caret is placed;
    ;; a space never fills, so the caret is never inside a run that a break eats.
    (define/private (auto-fill!)
      (define caret (get-start-position))
      (define para (position-paragraph caret))
      (define start (paragraph-start-position para))
      (define width (obs-peek @fill-column))
      (define edits
        (if (> (- (paragraph-end-position para) start) width)
            (auto-fill-edits (buffer-lines) para width)
            '()))
      (unless (null? edits)
        (begin-edit-sequence)
        (for ([e (in-list edits)])
          (insert (third e) (+ start (first e)) (+ start (second e))))
        (set-position (+ start (position-after-edits (- caret start) edits)))
        (end-edit-sequence)))
    (define/override (on-local-char e)
      (super on-local-char e)
      (define k (send e get-key-code))
      (when (and (obs-peek @auto-fill?)
                 (char? k)
                 (char-graphic? k)
                 (not (send e get-meta-down))
                 (not (send e get-control-down)))
        (auto-fill!)))))

;; The text block is centered, iA Writer style: the left padding is whatever
;; centers a wrap-column-wide block in the view, never less than min-margin,
;; and never less than a long file's line numbers need; the right margin is
;; bare view space of the same width. The gutter lives inside the left
;; padding, so toggling line numbers never moves the text.
(define min-margin 32)
(define line-number-gap 8)

;; Auto-wrap hands text% the view width, and text% lays lines out in that
;; less twice the left padding and a caret allowance (racket/gui 9.3), so
;; the column would fall short of the view less both margins by the
;; allowance; the slack in set-max-width below restores it. Right padding
;; would only widen the extent and summon a horizontal scrollbar, hence none.
(define wrap-slack 12)

;; A small gutter drawn in the text's left padding: smaller dimmed numbers in
;; the standard style's color (so color schemes still work), full-strength on
;; the caret's line, and no number on soft-wrap continuation rows.
;; framework's text:line-numbers-mixin is not used because its drawing
;; methods are sealed inside its unit and cannot be overridden.
(define (camp-line-numbers-mixin %)
  (class %
    (inherit get-dc get-admin get-canvas last-line line-location line-paragraph
             get-visible-line-range position-paragraph
             get-start-position get-end-position auto-wrap
             set-padding invalidate-bitmap-cache in-edit-sequence? get-style-list)
    ;; fields precede super-new: augments below fire during superclass init
    (define show? #t)
    (define padding-left 0)
    (super-new)
    ;; soft-wrap at the column: long tokens fold instead of scrolling sideways
    (auto-wrap #t)

    (define/public (show-line-numbers! on?)
      (set! show? on?)
      (setup-gutter!)
      (invalidate-bitmap-cache))
    (define/public (showing-line-numbers?) show?)

    (define/private (setup-gutter!)
      (define dc (get-dc))
      (define admin (get-admin))
      (define view-w
        (and dc admin
             (let ([w (box 0)])
               (send admin get-view #f #f w #f)
               (and (positive? (unbox w)) (unbox w)))))
      (define new-left
        (cond
          [view-w
           (define widest (number->string (max 10 (add1 (last-line)))))
           (define-values (nw _h _b _s)
             (send dc get-text-extent widest (obs-peek @gutter-font)))
           (define sl (get-style-list))
           (define-values (cw _ch _cd _ce)
             (send dc get-text-extent
                   "0" (send (or (send sl find-named-style "Standard") (send sl basic-style))
                             get-font)))
           (inexact->exact
            (ceiling (max min-margin
                          (+ nw line-number-gap)
                          (/ (- view-w (* cw (obs-peek @fill-column))) 2))))]
          [else min-margin]))
      (unless (= new-left padding-left)
        (set! padding-left new-left)
        (set-padding new-left 0 0 0)))

    ;; auto-wrap hands over the view width on every display-size
    (define/override (set-max-width w)
      (super set-max-width (if (real? w) (+ w wrap-slack) w)))

    (define/augment (on-display-size)
      (inner (void) on-display-size)
      (setup-gutter!))

    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (unless (in-edit-sequence?) (setup-gutter!)))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (unless (in-edit-sequence?) (setup-gutter!)))
    (define/augment (after-edit-sequence)
      (setup-gutter!)
      (inner (void) after-edit-sequence))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (when (zero? padding-left) (setup-gutter!))
        (when show? (draw-gutter dc dx dy top bottom))))

    (define/private (draw-gutter dc dx dy top bottom)
      (define num-font (obs-peek @gutter-font))
      (define saved-font (send dc get-font))
      (define saved-fg (send dc get-text-foreground))
      (define saved-alpha (send dc get-alpha))
      (define saved-mode (send dc get-text-mode))
      (send dc set-font num-font)
      (send dc set-text-mode 'transparent)
      ;; the default-color style, not "Standard", carries the scheme's text color
      (define sl (get-style-list))
      (define std-style (or (send sl find-named-style (editor:get-default-color-style-name))
                            (send sl basic-style)))
      (send dc set-text-foreground (send std-style get-foreground))
      ;; align number baselines with the text baseline
      (define-values (_sw std-h std-desc _se)
        (send dc get-text-extent "0" (send std-style get-font)))
      (define-values (_nw num-h num-desc _ne)
        (send dc get-text-extent "0" num-font))
      (define y-offset (- (- std-h std-desc) (- num-h num-desc)))
      (define view-left
        (let ([l (box 0)] [t (box 0)] [w (box 0)] [h (box 0)])
          (send (get-admin) get-view l t w h)
          (unbox l)))
      (define right-x (+ view-left dx padding-left (- line-number-gap)))
      ;; opaque background so text scrolled right doesn't show through
      (define bg (let ([cv (get-canvas)]) (and cv (send cv get-canvas-background))))
      (when bg
        (define saved-pen (send dc get-pen))
        (define saved-brush (send dc get-brush))
        (send dc set-pen bg 1 'transparent)
        (send dc set-brush bg 'solid)
        (send dc draw-rectangle (+ view-left dx) (+ dy top) padding-left (- bottom top))
        (send dc set-pen saved-pen)
        (send dc set-brush saved-brush))
      (define caret-para
        (let ([sp (get-start-position)])
          (and (= sp (get-end-position)) (position-paragraph sp))))
      (define-values (first-line last-vis-line)
        (let ([s (box 0)] [e (box 0)])
          (get-visible-line-range s e)
          (values (unbox s) (unbox e))))
      (for/fold ([prev-para #f]) ([line (in-range first-line (add1 last-vis-line))])
        (define para (line-paragraph line))
        (unless (equal? para prev-para)
          (define label (number->string (add1 para)))
          (define-values (w _h _b _s) (send dc get-text-extent label))
          (send dc set-alpha (if (equal? para caret-para) saved-alpha (* saved-alpha 0.45)))
          (send dc draw-text label (- right-x w) (+ dy (line-location line) y-offset)))
        para)
      (send dc set-alpha saved-alpha)
      (send dc set-text-foreground saved-fg)
      (send dc set-text-mode saved-mode)
      (send dc set-font saved-font))))

;; The caret is one fixed light blue in both polarities: the insertion caret
;; (vim insert mode, and always without vim) is a bar painted over text%'s
;; hairline, which has no color or width of its own, and the vim block cursor
;; is a solid cell of it with the glyph beneath redrawn in black. The vim tool
;; draws its block as a "slategray" highlight, or, at a line end, as its own
;; rectangle in on-paint (in visual mode at its private anchor, which is then
;; whichever selection end sits on an empty line); both are painted over here.
;; Its visual selection is a "lightsteelblue" highlight, swapped for the
;; scheme's selection color.
(define caret-color (make-color 31 190 255))
(define caret-bar-width 2)

(define (cursor-mixin %)
  (class %
    (inherit get-start-position get-end-position position-location
             caret-hidden? invalidate-bitmap-cache vim?
             position-line line-start-position line-end-position
             find-snip get-snip-location get-character get-style-list)
    (super-new)
    (define block-pos #f)
    (define/override (highlight-range start end color
                                      [caret-space? #f] [priority 'low] [style 'rectangle]
                                      #:adjust-on-insert/delete? [adjust? #f]
                                      #:key [key #f])
      (cond
        [(and (eq? key 'drracket-vim-highlight) (equal? color "slategray"))
         (set! block-pos start)]
        [else
         (super highlight-range start end
                (if (and (eq? key 'drracket-vim-highlight) (equal? color "lightsteelblue"))
                    (obs-peek @vim-selection-color)
                    color)
                caret-space? priority style
                #:adjust-on-insert/delete? adjust? #:key key)]))
    (define/override (unhighlight-ranges/key key)
      (when (eq? key 'drracket-vim-highlight) (set! block-pos #f))
      (super unhighlight-ranges/key key))

    (define (at-line-end? pos) (= pos (line-end-position (position-line pos))))
    (define (on-empty-line? pos) (= pos (line-start-position (position-line pos))
                                    (line-end-position (position-line pos))))
    (define (block-positions)
      (define start (get-start-position))
      (define end (get-end-position))
      (cond
        [block-pos (list block-pos)]
        [(not (and (vim?) (caret-hidden?))) '()]
        [(= start end) (if (at-line-end? start) (list start) '())]
        [else (filter on-empty-line? (remove-duplicates (list start end)))]))

    ;; The cell at pos as (x top bottom) in editor coordinates
    (define (cell pos)
      (define x (box 0))
      (define top (box 0))
      (define bottom (box 0))
      (position-location pos x top #t #f #t)
      (position-location pos #f bottom #f #f #t)
      (values (unbox x) (unbox top) (unbox bottom)))

    ;; The bar at pos as (x y w h), straddling the hairline's column
    (define (caret-bar pos)
      (define-values (x top bottom) (cell pos))
      (values (- x 0.5) top caret-bar-width (- bottom top)))

    (define (draw-block dc dx dy pos)
      (define-values (x top bottom) (cell pos))
      (define snip (and (not (at-line-end? pos)) (find-snip pos 'after-or-none)))
      (define font (send (if snip (send snip get-style) (send (get-style-list) basic-style)) get-font))
      (define width
        (cond
          [snip (let-values ([(nx _t _b) (cell (add1 pos))]) (- nx x))]
          [else (let-values ([(w _h _d _s) (send dc get-text-extent "a" font)]) w)]))
      (send dc set-brush caret-color 'solid)
      (send dc draw-rectangle (+ x dx) (+ top dy) width (- bottom top))
      (when snip
        (define sy (box 0))
        (get-snip-location snip #f sy)
        (send dc set-font font)
        (send dc draw-text (string (get-character pos)) (+ x dx) (+ (unbox sy) dy))))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (define pos (get-start-position))
        (define bar?
          (and (eq? draw-caret 'show-caret)
               (not (caret-hidden?))
               (= pos (get-end-position))))
        (define blocks (block-positions))
        (when (or bar? (pair? blocks))
          (define pen (send dc get-pen))
          (define brush (send dc get-brush))
          (define font (send dc get-font))
          (define fg (send dc get-text-foreground))
          (define mode (send dc get-text-mode))
          (send dc set-pen "black" 0 'transparent)
          (send dc set-text-foreground "black")
          (send dc set-text-mode 'transparent)
          (when bar?
            (define-values (x y w h) (caret-bar pos))
            (when (and (<= x right) (<= left (+ x w)) (<= y bottom) (<= top (+ y h)))
              (send dc set-brush caret-color 'solid)
              (send dc draw-rectangle (+ x dx) (+ y dy) w h)))
          (for ([p (in-list blocks)])
            (draw-block dc dx dy p))
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc set-font font)
          (send dc set-text-foreground fg)
          (send dc set-text-mode mode))))

    ;; text% refreshes only its hairline when the caret moves; refresh the
    ;; bar's full width where it was and where it is
    (define last-pos #f)
    (define (refresh-bar!)
      (define pos (get-start-position))
      (for ([p (in-list (if (and last-pos (not (= last-pos pos))) (list last-pos pos) (list pos)))])
        (define-values (x y w h) (caret-bar p))
        (invalidate-bitmap-cache (- x 1) y (+ w 2) h))
      (set! last-pos pos))
    (define/augment (after-set-position)
      (inner (void) after-set-position)
      (refresh-bar!))
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (refresh-bar!))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (refresh-bar!))))

;; Vim's command mode matches raw key codes, so ⌘-shortcuts would otherwise be
;; parsed as vim commands (⌘S as `s`); give them to the camp keymap first
(define (command-keys-mixin %)
  (class %
    (super-new)
    (define/override (on-local-char e)
      (unless (and (send e get-meta-down)
                   (send camp-editor-keymap handle-key-event this e))
        (super on-local-char e)))))

(define camp-editor-text%
  (command-keys-mixin
   (cursor-mixin
    (vim-emulation-mixin
     (text:searching-mixin
      (camp-line-numbers-mixin
       (editor:autoload-mixin
        (camp-editor-mixin racket:text%))))))))

;; ============================================================================
;; Editor windows

(define open-editors (make-hash))

(define (open-editor! path #:on-save [on-save void])
  (define key (simple-form-path path))
  (cond
    [(hash-ref open-editors key #f)
     => (λ (r)
          (define frame (renderer-root r))
          (send frame show #t)
          (send frame focus))]
    [else (open-new-editor! key on-save)]))

(define (confirm-close! ed frame)
  (or (not (send ed is-modified?))
      (case (message-box/custom
             "Unsaved Changes"
             (format "Save changes to ~a?" (file-name-from-path (send ed get-filename)))
             "Save" "Discard" "Cancel"
             frame
             '(default=1 caution))
        [(1) (send ed save-file #f 'text)
             (not (send ed is-modified?))]
        [(2) #t]
        [else #f])))

(define (editor-frames)
  (for/list ([r (in-hash-values open-editors)]) (renderer-root r)))

;; Closes every editor window, offering to save each modified buffer; #f if
;; the user cancels, which leaves that window and any after it open
(define (close-all-editors!)
  (for/and ([f (in-list (editor-frames))])
    (send f close-editor-window!)))

(define (find-editor-canvas w)
  (cond
    [(is-a? w editor-canvas%) w]
    [(is-a? w area-container<%>)
     (for/or ([c (in-list (send w get-children))])
       (find-editor-canvas c))]
    [else #f]))

(define (open-new-editor! key on-save)
  (define/obs @dirty? #f)
  (define/obs @status "")
  (define/obs @saved "")
  (define/obs @find-text "")
  (define/obs @find-visible? #f)
  (define @title (obs-map @dirty? (λ (d) (editor-title key d))))
  (define @modified (obs-map @dirty? (λ (d) (if d "Modified" ""))))
  (define ed
    (new camp-editor-text%
         [@dirty? @dirty?]
         [on-save-cb (λ (fn)
                       (@saved . := . (saved-message (send ed get-text)
                                                     (file-size fn)
                                                     (~t (now) "HH:mm MMM d")))
                       (on-save fn))]))
  (send ed load-file key 'text)
  (send ed refresh-completions)
  (send ed refresh-lang-info!)
  (send ed show-line-numbers! (obs-peek @line-numbers?))
  ;; wide enough to center a wrap-column-wide block between two min-margins,
  ;; plus the scrollbar
  (define window-width
    (let* ([sl (editor:get-standard-style-list)]
           [font (send (or (send sl find-named-style "Standard") (send sl basic-style))
                       get-font)]
           [dc (new bitmap-dc% [bitmap (make-bitmap 1 1)])])
      (define-values (cw _h _d _e) (send dc get-text-extent "0" font))
      (inexact->exact (ceiling (+ (* cw (obs-peek @fill-column)) (* 2 min-margin) 40)))))
  (define r #f)
  (define find-field (box #f))

  (define (update-search! s)
    (send ed set-searching-state (and (non-empty-string? s) s) #f #f))
  (define (find-next!)
    (define str (obs-peek @find-text))
    (when (non-empty-string? str)
      (define hit
        (or (send ed find-string str 'forward (send ed get-end-position) 'eof #t #f)
            (send ed find-string str 'forward 0 'eof #t #f)))
      (when hit
        (send ed set-position hit (+ hit (string-length str))))))
  (define (focus-editor!)
    (define c (find-editor-canvas (renderer-root r)))
    (when c (send c focus)))

  (define (vim-sync on?)
    (queue-callback
     (λ ()
       (cond
         [on? (send ed on-initialization)]
         [else (send ed turn-off-vim-effects!)
               (@status . := . "")]))))
  (define (line-numbers-sync on?)
    (queue-callback (λ () (send ed show-line-numbers! on?))))
  ;; a font-slot change resizes the gutter, so re-measure its padding
  (define (gutter-font-sync _font)
    (queue-callback (λ () (send ed show-line-numbers! (obs-peek @line-numbers?)))))
  ;; gui-easy's editor-canvas doesn't use framework's canvas:color-mixin, so
  ;; the scheme's background is applied here; the gutter paints from it
  (define (canvas-bg-sync color)
    (queue-callback
     (λ ()
       (define canvas (find-editor-canvas (renderer-root r)))
       (when canvas
         (send canvas set-canvas-background color)
         (send canvas refresh)))))

  (define (editor-window-mixin %)
    (class %
      (super-new)
      (define/public (vim?) (obs-peek @vim-mode))
      ;; the vim tool's "-- INSERT --" style, without the dashes
      (define/public (set-vim-status-message s)
        (@status . := . (regexp-replace* #px"^-- | --$" s "")))
      ;; Stubs for the vim tool's tab/window ex-commands
      (define/public (next-tab) (void))
      (define/public (prev-tab) (void))
      (define/public (close-current-tab) (void))
      (define/public (open-in-new-tab _filename) (void))
      (define/public (move-current-tab-right) (void))
      (define/public (move-current-tab-left) (void))
      (define/public (get-definitions-canvas) (find-editor-canvas this))
      (define/public (get-interactions-canvas) (find-editor-canvas this))
      ;; gui-easy shows the field a turn or two after the observable flips
      (define/public (focus-find-field!)
        (@find-visible? . := . #t)
        (let retry ([turns 10])
          (queue-callback
           (λ ()
             (define f (unbox find-field))
             (cond [(and f (send f is-shown?)) (send f focus)]
                   [(positive? turns) (retry (sub1 turns))]))
           #f)))
      (define/public (close-editor-window!)
        (and (send this can-close?)
             (begin (send this on-close)
                    (send this show #f)
                    #t)))
      (define/augment (can-close?)
        (and (confirm-close! ed this) (inner #t can-close?)))
      ;; renderer-destroy is deferred: destroying the renderer from inside
      ;; its own frame's on-close re-enters gui-easy mid-teardown
      (define/augment (on-close)
        (hash-remove! open-editors key)
        ;; framework frames send this; without it the autoload monitor keeps
        ;; watching the file on behalf of the dead window
        (send ed on-close)
        (obs-unobserve! @vim-mode vim-sync)
        (obs-unobserve! @line-numbers? line-numbers-sync)
        (obs-unobserve! @gutter-font gutter-font-sync)
        (obs-unobserve! @canvas-bg canvas-bg-sync)
        (queue-callback (λ () (renderer-destroy r)) #f)
        (inner (void) on-close))))

  ;; Frame menus: every editor command with a key combo is listed so the
  ;; combos are discoverable. No shortcut may involve ⌥: macOS delivers
  ;; option chords with a translated key-code ("ƒ" for ⌥F) and both keymaps
  ;; and menu shortcuts (wxmenu.rkt dispatches them through a keymap%, not
  ;; native NSMenu key equivalents) fail to match it.
  ;; Generic edit operations target the focused control, not always ed.
  (define (edit-op op)
    (λ ()
      (define target (send (renderer-root r) get-edit-target-object))
      (when (and target (is-a? target editor<%>))
        (send target do-edit-operation op))))
  (define editor-menu-bar
    (menu-bar
     (menu "File"
           (menu-item "Save" (λ () (send ed save-file #f 'text))
                      #:shortcut '(cmd #\S))
           (menu-item-separator)
           (menu-item "Close" (λ () (send (renderer-root r) close-editor-window!))
                      #:shortcut '(cmd #\W)))
     (menu "Edit"
           (menu-item "Undo" (edit-op 'undo) #:shortcut '(cmd #\Z))
           (menu-item "Redo" (edit-op 'redo) #:shortcut '(cmd shift #\Z))
           (menu-item-separator)
           (menu-item "Cut" (edit-op 'cut) #:shortcut '(cmd #\X))
           (menu-item "Copy" (edit-op 'copy) #:shortcut '(cmd #\C))
           (menu-item "Paste" (edit-op 'paste) #:shortcut '(cmd #\V))
           (menu-item "Select All" (edit-op 'select-all) #:shortcut '(cmd #\A))
           (menu-item-separator)
           (menu-item "Re-wrap Paragraph" (λ () (send ed fill-paragraph!))
                      #:shortcut '(cmd #\J))
           (menu-item-separator)
           (menu-item "Find" (λ () (send (renderer-root r) focus-find-field!))
                      #:shortcut '(cmd #\F)))
     (menu "View"
           (checkable-menu-item "Line Numbers"
                                (λ (on?) (@line-numbers? . := . (and on? #t)))
                                #:checked? @line-numbers?
                                #:shortcut '(cmd shift #\L))
           (menu-item-separator)
           (menu-item "Cycle Editor Font" (λ () (cycle-font-slot!))
                      #:shortcut '(cmd shift #\F)))))
  (set! r
        (render
         (window
          #:title @title
          #:size (list window-width 820)
          #:mixin (λ (%) ((remember-geometry-mix @editor-geometry #:others editor-frames)
                          (editor-window-mixin %)))
          editor-menu-bar
          (editor-canvas ed #:style '(auto-hscroll auto-vscroll) #:inset '(0 8))
          ;; Status bar: the Modified badge's slot, then the find field or the
          ;; last save report, then the vim mode badge at the right end
          (hpanel #:stretch '(#t #f)
                  #:spacing 12
                  #:margin '(12 4)
                  (badge @modified "Modified")
                  (if-view @find-visible?
                           (input @find-text
                                  (λ (action s)
                                    (@find-text . := . s)
                                    (case action
                                      [(input) (update-search! s)]
                                      [(return) (find-next!)]))
                                  #:label "Find:"
                                  #:stretch '(#f #f)
                                  #:min-size '(240 #f)
                                  #:mixin (λ (%)
                                            (class %
                                              (super-new)
                                              (set-box! find-field this)
                                              (define/override (on-subwindow-char rcv ev)
                                                (cond
                                                  [(eq? (send ev get-key-code) 'escape)
                                                   (@find-text . := . "")
                                                   (update-search! "")
                                                   (@find-visible? . := . #f)
                                                   (focus-editor!)
                                                   #t]
                                                  [else (super on-subwindow-char rcv ev)])))))
                           (text @saved))
                  (badge @status "VISUAL-LINE" #:align 'right #:stretch? #t)))))
  (set-field! parent-frame ed (renderer-root r))
  (canvas-bg-sync (obs-peek @canvas-bg))
  (obs-observe! @vim-mode vim-sync)
  (obs-observe! @line-numbers? line-numbers-sync)
  (obs-observe! @gutter-font gutter-font-sync)
  (obs-observe! @canvas-bg canvas-bg-sync)
  (when (obs-peek @vim-mode) (send ed on-initialization))
  (hash-set! open-editors key r)
  (log-msg "Editing: ~a" (file-name-from-path key)))
