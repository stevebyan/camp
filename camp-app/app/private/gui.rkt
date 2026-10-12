#lang racket/base

;; Shared GUI components: log textbox, dialogs, mixins

(require racket/gui
         camp/log
         (only-in camp/private/log use-color?)
         camp/private/ansi
         camp/app/private/window-geometry
         (only-in mrlib/panel-wob white-on-black-panel-scheme?)
         racket/class
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/string)

(use-color? #t)

(provide badge
         draw-badge
         dragdrop-mix
         remember-geometry-mix
         make-mix-close
         ?dialog
         choose-font
         set-main-frame!
         get-main-frame
         mono-font
         log-msg
         log-output-textbox
         log-output-textbox%)

(define mono-font (make-object font% 12 "Menlo" 'modern))

;; ============================================================================
;; Badges
;;
;; A monospace label on a rounded tint, for status bars. A badge view's slot
;; is sized for the widest label it will show, so the layout never shifts
;; as the label changes or clears; a right-aligned badge may also stretch
;; to take the remaining width.

(define badge-pad 6)

(define (badge-size label)
  (define dc (new bitmap-dc% [bitmap (make-bitmap 1 1)]))
  (define-values (w h _d _e) (send dc get-text-extent label mono-font))
  (values (inexact->exact (ceiling (+ w (* 2 badge-pad))))
          (inexact->exact (ceiling (+ h 4)))))

(define (draw-badge dc label align)
  (unless (string=? label "")
    (define-values (cw ch) (send dc get-size))
    (define-values (tw th _d _e) (send dc get-text-extent label mono-font))
    (define bw (+ tw (* 2 badge-pad)))
    (define bh (+ th 4))
    (define x (if (eq? align 'right) (- cw bw) 0))
    (define y (/ (- ch bh) 2))
    (define fg (get-label-foreground-color))
    (send dc set-smoothing 'smoothed)
    (send dc set-pen fg 1 'transparent)
    (send dc set-brush (make-color (send fg red) (send fg green) (send fg blue) 0.12) 'solid)
    (send dc draw-rounded-rectangle x y bw bh 4)
    (send dc set-font mono-font)
    (send dc set-text-foreground fg)
    (send dc draw-text label (+ x badge-pad) (+ y 2))))

(define (badge @label widest #:align [align 'left] #:stretch? [stretch? #f])
  (define-values (w h) (badge-size widest))
  (canvas @label
          #:style '(transparent)
          #:min-size (list w h)
          #:stretch (list stretch? #f)
          (λ (dc label) (draw-badge dc label align))))

;; ============================================================================
;; Logging helper

(define (log-msg fmt . args)
  (log-camp-info (apply format fmt args)))

;; ============================================================================
;; Log output textbox
;;
;; A styled terminal-like text display that shows log messages from the camp logger

(define monitor-thread
  (thread
   (λ ()
     (define listener (make-log-receiver camp-logger 'info))
     (let loop ()
       (sync/enable-break
        (handle-evt listener
                    (λ (vec)
                      (console-print (string-trim (vector-ref vec 1) "camp: " #:right? #f))
                      (loop))))))))

(define max-text-size 10240)
(define max-buffer-size 1000)

(define/obs @log-buffer '("Welcome to Camp!\n"))

(define (console-print str)
  (obs-update!
   @log-buffer
   (λ (buffer-lst)
     (define next-buffer (cons (string-append str "\n") buffer-lst))
     (if (> (length next-buffer) max-buffer-size)
         (take next-buffer max-buffer-size)
         next-buffer))))

(define log-output-textbox%
  (class* object% (view<%>)
    (init-field @buffer)
    (super-new)

    (define/public (dependencies)
      (list @buffer))

    (define/public (create parent)
      (define editor
        (new (class text%
               (super-new)
               (define allow-change? #f)
               (define/public (begin-allow-change)
                 (set! allow-change? #t))
               (define/public (end-allow-change)
                 (set! allow-change? #f))
               (define/augment (can-delete? _start _len)
                 allow-change?)
               (define/augment (can-insert? _start _len)
                 allow-change?))))

      ;; Standard bindings (⌘C copy, ⌘A select all, …) and the right-click
      ;; editor-operations menu; without a keymap a bare text% has neither
      (define keymap (new keymap%))
      ((current-text-keymap-initializer) keymap)
      (send editor set-keymap keymap)

      (define s-l (send editor get-style-list))
      (define basic-style (send s-l find-named-style (send editor default-style-name)))
      (define mono-delta (make-object style-delta% 'change-size 11))
      (send mono-delta set-face "Menlo")
      (send mono-delta set-delta-background "Black")
      (send mono-delta set-delta-foreground "LightGray")

      (define mono-style
        (send s-l find-or-create-style basic-style mono-delta))
      (send editor change-style mono-style)

      ;; Style deltas for ANSI colors
      (define (make-fg-delta color-name)
        (define d (make-object style-delta%))
        (send d set-delta-foreground color-name)
        d)
      (define bold-delta
        (let ([d (make-object style-delta%)])
          (send d set-delta-foreground "White")
          (send d set-weight-on 'bold)
          d))
      (define style-deltas
        (hasheq 'red     (make-fg-delta "Red")
                'green   (make-fg-delta "Green")
                'yellow  (make-fg-delta "Yellow")
                'blue    (make-fg-delta "RoyalBlue")
                'magenta (make-fg-delta "Magenta")
                'cyan    (make-fg-delta "Cyan")
                'white   (make-fg-delta "White")
                'dim     (make-fg-delta "Gray")
                'bold    bold-delta
                'bright-green (make-fg-delta "LightGreen")
                'bright-cyan  (make-fg-delta "LightCyan")))

      (define default-delta (make-fg-delta "LightGray"))

      (define (insert-styled str)
        (for ([span (in-list (parse-ansi str))])
          (define text (car span))
          (define style (cdr span))
          (define start (send editor last-position))
          (send editor insert text)
          (define delta (if style (hash-ref style-deltas style default-delta) default-delta))
          (send editor change-style delta start (send editor last-position))))

      (send editor begin-allow-change)
      (insert-styled (string-join (reverse (obs-peek @buffer))))
      (send editor end-allow-change)

      (define canvas
        (new (context-mixin editor-canvas%)
             [parent parent]
             [editor editor]))
      (begin0 canvas
              (send canvas set-canvas-background (make-object color% 0 0 0))
              (send canvas set-context 'buffer (obs-peek @buffer))
              (send canvas set-context 'insert-styled insert-styled)))

    (define/public (update v dep val)
      (case/dep dep
                [@buffer
                 (define old (send v get-context 'buffer))
                 (define todo
                   (reverse
                    (for/list ([chunk (in-list val)])
                      #:break (and (not (null? old)) (eq? chunk (car old)))
                      chunk)))
                 (unless (null? todo)
                   (define editor (send v get-editor))
                   (define insert-styled (send v get-context 'insert-styled))
                   (send editor begin-edit-sequence)
                   (send editor begin-allow-change)
                   (send editor set-position (send editor last-position))
                   (for ([chunk (in-list todo)])
                     (insert-styled chunk))
                   (define last-pos (send editor last-position))
                   (send editor scroll-to-position last-pos #f 'same 'end)
                   (when (> last-pos max-text-size)
                     (send editor delete 0 (quotient max-text-size 2)))
                   (send editor end-allow-change)
                   (send editor end-edit-sequence)
                   (send v set-context 'buffer val))]))

    (define/public (destroy v)
      (send v clear-context))))

(define (log-output-textbox)
  (new log-output-textbox% [@buffer @log-buffer]))

;; ============================================================================
;; Main frame reference (for centering dialogs)

(define main-frame #f)
(define (set-main-frame! f) (set! main-frame f))
(define (get-main-frame) main-frame)

;; Mixin: reparent dialogs to the main frame so built-in `center` handles
;; multi-monitor positioning correctly.
(define (reparent-to-main-mix %)
  (class %
    (init [parent #f])
    (super-new [parent (or main-frame parent)])))

;; ============================================================================
;; Closable dialogs

(define (make-mix-close)
  (define close-proc! void)
  (define (mix %)
    (reparent-to-main-mix
     (class %
       (super-new)
       (set! close-proc!
             (lambda ()
               (send this show #f))))))
  (values (λ () (close-proc!)) mix))

(define (?dialog msg)
  (define-values (close! closing-mixin) (make-mix-close))
  (dialog
   #:title msg
   #:mixin closing-mixin
   (vpanel
    (text msg)
    (button "Close" close!))))

;; gui-lib's font dialog replaces its sample's "Standard" delta on every
;; change, losing the white foreground text-field% gives it in dark mode. A
;; delta on the sample text itself outlives the replacements; the callback
;; runs once the modal dialog is up.
(define (choose-font parent font)
  (when (white-on-black-panel-scheme?)
    (queue-callback
     (λ ()
       (for* ([w (in-list (get-top-level-windows))]
              #:when (equal? (send w get-label) "Choose Font")
              [c (in-list (send w get-children))]
              #:when (is-a? c text-field%))
         (send (send c get-editor) change-style
               (send (make-object style-delta%) set-delta-foreground "white")
               0 'end)))
     #f))
  (get-font-from-user #f parent font))

;; ============================================================================
;; Window geometry

;; A frame mixin that keeps the window's placement in @geom and puts the
;; window back when it is next shown. Placement is kept per monitor layout:
;; @geom holds a short history of (screens x y w h) entries, most recent
;; first, where screens is the list of screen frames at the time, so a
;; laptop that has gone from two monitors to one and back finds its
;; two-monitor spot again. Without an entry for the current layout, the
;; most recent one is used when its title bar still lands on an attached
;; screen; otherwise the frame keeps its default centering. When another
;; of the frames `others` lists already sits at the spot, the window
;; cascades down and right of it. Moves and resizes stream in during a
;; drag, so the save is debounced.
(define layout-history 8)

(define (title-bar-on-screen? x y w)
  (for/or ([screen (in-list (screen-frames))])
    (match-define (list left top sw sh) screen)
    (and (<= left (+ x w -100))
         (<= (+ x 100) (+ left sw))
         (<= top y)
         (<= (+ y 40) (+ top sh)))))

(define (remember-geometry-mix @geom #:others [others (λ () '())])
  (λ (%)
    (class %
      (define (entries)
        (define v (obs-peek @geom))
        (if (and (list? v) (andmap pair? v)) v '()))
      (define (save!)
        (define screens (screen-frames))
        (define kept (filter (λ (e) (not (equal? (car e) screens))) (entries)))
        (@geom . := . (cons (cons screens (window-frame this))
                            (take kept (min layout-history (length kept))))))
      (define saver (new timer% [notify-callback save!]))
      (define (save-soon!) (send saver start 250 #t))
      (define shown? #f)
      (super-new)
      (inherit resize)

      (define (restore!)
        (define saved (entries))
        (match (or (assoc (screen-frames) saved) (and (pair? saved) (car saved)))
          [(list _ x y w h)
           (define-values (sw sh) (primary-visible-size))
           (resize (min w sw) (min h sh))
           (define taken (for/list ([f (in-list (others))]) (take (window-frame f) 2)))
           (define-values (fx fy)
             (let free ([x x] [y y])
               (if (member (list x y) taken) (free (+ x 22) (+ y 22)) (values x y))))
           (when (title-bar-on-screen? fx fy w)
             (set-window-top-left! this fx fy))]
          [_ (void)]))
      ;; gui-easy centers the frame between construction and its first show
      (define/override (show on?)
        (when (and on? (not shown?))
          (set! shown? #t)
          (restore!))
        (super show on?))

      (define/override (on-move x y)
        (super on-move x y)
        (save-soon!))
      (define/override (on-size w h)
        (super on-size w h)
        (save-soon!))
      (define/override (on-exit)
        (send saver stop)
        (save!)
        (super on-exit))
      (define/augment (on-close)
        (send saver stop)
        (save!)
        (inner (void) on-close)))))

;; ============================================================================
;; Drag and drop mixin

(define (dragdrop-mix %)
  (class %
    (super-new)
    (send this accept-drop-files #t)
    (define/override (on-drop-file p)
      (log-msg "Dropped: ~a" (path->string p)))))
