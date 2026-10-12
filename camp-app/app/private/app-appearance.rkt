#lang racket/base

;; The app's Light/Dark override for system-drawn parts (the selection color,
;; scrollbars, window chrome), kept apart from appearance.rkt so the main
;; window gets it before framework loads.

(require racket/gui/easy
         camp/app/private/settings)

(provide init-app-appearance!)

(define set-app-appearance!
  (if (eq? (system-type 'os) 'macosx)
      (dynamic-require 'camp/app/private/mac-appearance 'set-app-appearance!)
      void))

;; Observers run in subscription order, so calling this before
;; init-appearance! also settles the app's appearance before framework's
;; color scheme follows it
(define (init-app-appearance!)
  (define (apply!) (set-app-appearance! (obs-peek @appearance-mode)))
  (apply!)
  (obs-observe! @appearance-mode (λ (_) (apply!))))
