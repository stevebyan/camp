#lang racket/base

;; Tests for the file watcher. It polls modification times rather than
;; holding a filesystem change event (on macOS, an open file descriptor) per
;; path, so its cost does not grow with the site; and its watch paths may be a
;; thunk, which lets a long-running watcher pick up path changes (new files,
;; reconfigured source folders) without being restarted.

(require rackunit
         racket/file
         racket/path
         (only-in camp/private/watch start-watcher!))

(define (write! p [content "x"])
  (display-to-file content p #:exists 'truncate))

;; Starts a watcher over the files of a fresh directory and hands the test
;; the directory and a procedure that awaits the next reported path
(define (call-with-watched-dir files proc #:on-change [on-change void])
  (define dir (make-temporary-directory "watch-test-~a"))
  (for ([f (in-list files)])
    (write! (build-path dir f)))
  (define changes (make-channel))
  (define stop
    (start-watcher! (λ () (directory-list dir #:build? #t))
                    (λ (p) (on-change p) (channel-put changes p))
                    #:interval-ms 25))
  (proc dir (λ ([timeout 2]) (sync/timeout timeout changes)))
  (stop)
  (delete-directory/files dir))

;; ---------------------------------------------------------------------------
;; List-valued paths: starts and stops cleanly

(define quiet-dir (make-temporary-directory "watch-test-quiet-~a"))
(define stop-quiet (start-watcher! (list quiet-dir) void))
(check-pred procedure? stop-quiet)
(stop-quiet)
(delete-directory/files quiet-dir)

;; ---------------------------------------------------------------------------
;; Changed, new and deleted files are each reported by their own path, and
;; nothing is reported while nothing changes

(call-with-watched-dir
 '("a.txt" "b.txt")
 (λ (dir next-change)
   (check-false (next-change 0.2) "no report without a change")

   (write! (build-path dir "b.txt") "edited")
   (check-equal? (next-change) (build-path dir "b.txt"))

   (write! (build-path dir "c.txt"))
   (check-equal? (next-change) (build-path dir "c.txt"))

   (delete-file (build-path dir "a.txt"))
   (check-equal? (next-change) (build-path dir "a.txt"))

   (check-false (next-change 0.2) "each change is reported once")))

;; ---------------------------------------------------------------------------
;; A directory among the paths reports entries added to it

(let ([dir (make-temporary-directory "watch-test-dir-~a")]
      [changes (make-channel)])
  (define stop
    (start-watcher! (list dir) (λ (p) (channel-put changes p)) #:interval-ms 25))
  (write! (build-path dir "new.txt"))
  (check-equal? (sync/timeout 2 changes) dir)
  (stop)
  (delete-directory/files dir))

;; ---------------------------------------------------------------------------
;; An edit made while a change is being handled (a build in progress) is
;; reported once the handler returns

(let ([building (make-semaphore 0)]
      [resume (make-semaphore 0)])
  (call-with-watched-dir
   '("a.txt" "b.txt")
   #:on-change (λ (p)
                 (when (equal? (file-name-from-path p) (string->path "a.txt"))
                   (semaphore-post building)
                   (semaphore-wait resume)))
   (λ (dir next-change)
     (write! (build-path dir "a.txt") "edited")
     (check-not-false (sync/timeout 2 building) "the first edit must reach the handler")
     (write! (build-path dir "b.txt") "edited during the build")
     (semaphore-post resume)
     (check-equal? (next-change) (build-path dir "a.txt"))
     (check-equal? (next-change) (build-path dir "b.txt")))))

;; ---------------------------------------------------------------------------
;; Watching holds no file descriptors, however many paths there are

(when (directory-exists? "/dev/fd")
  (define (open-descriptors) (length (directory-list "/dev/fd")))
  (define before (open-descriptors))
  (call-with-watched-dir
   (for/list ([i (in-range 500)]) (format "file-~a.txt" i))
   (λ (dir next-change)
     (write! (build-path dir "file-250.txt") "edited")
     (check-equal? (next-change) (build-path dir "file-250.txt"))
     (check-true (< (- (open-descriptors) before) 20)
                 "descriptors in use must not grow with the watch list"))))
