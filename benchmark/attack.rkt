#lang racket/base

(require racket/tcp)

(define (write* bs out)
  (write-bytes bs out)
  (flush-output out))

(define (expect in s)
  (define s-read (read-line in 'return-linefeed))
  (unless (equal? s-read s)
    (error 'expect "expected ~s but received ~s" s s-read)))

(define (start-attacker stats-ch)
  (let outer-loop ()
    (with-handlers ([exn:break? void]
                    [exn:fail? (λ (err)
                                 ((error-display-handler) (exn-message err) err)
                                 (channel-put stats-ch 'err)
                                 (outer-loop))])
      (define-values (in out)
        (tcp-connect "127.0.0.1" 8675))
      (void (read-line in))
      (let loop ()
        (write* #"MAIL FROM:<attacker@example.com>\r\n" out)
        (expect in "250 OK")
        (write* #"RCPT TO:<bogdan@defn.io>\r\n" out)
        (expect in "250 OK")
        (write* #"DATA\r\n" out)
        (expect in "354 end data with <CRLF>.<CRLF>")
        (write* #"Hello, world!\r\n.\r\n" out)
        (expect in "250 OK")
        (channel-put stats-ch 'ok)
        (loop)))))

(define (attack! concurrency duration)
  (define stats-ch (make-channel))
  (define stats-thd
    (thread
     (lambda ()
       (let loop ([total 0] [failed 0] [alarm (alarm-evt (+ (current-inexact-milliseconds) 1000))])
         (sync
          (handle-evt
           alarm
           (lambda (_)
             (printf "total: ~a failed: ~a~n" total failed)
             (loop total failed (alarm-evt (+ (current-inexact-milliseconds) 1000)))))
          (handle-evt
           stats-ch
           (lambda (message)
             (case message
               [(ok) (loop (add1 total) failed alarm)]
               [(err) (loop (add1 total) (add1 failed) alarm)]))))))))
  (define thds
    (for/list ([_ (in-range concurrency)])
      (thread
       (lambda ()
         (start-attacker stats-ch)))))
  (sleep duration)
  (for-each break-thread thds)
  (for-each thread-wait thds)
  (break-thread stats-thd))

(module+ main
  (require racket/cmdline)
  (define-values (concurrency duration)
    (let ([concurrency 1000]
          [duration 60])
      (command-line
       #:once-each
       [("--concurrency" "-c") CONCURRENCY "the number of concurrent connections to run"
                               (define concurrency-num (string->number CONCURRENCY))
                               (unless concurrency
                                 (eprintf "error: CONCURRENCY must be a positive integer~n")
                                 (exit 1))
                               (set! concurrency concurrency-num)]
       [("--duration" "-d") DURATION "how long to attack the server for (in seconds)"
                            (define duration-num (string->number DURATION))
                            (unless (and duration (> duration 0))
                              (eprintf "error: DURATION must be a positive integer~n")
                              (exit 1))
                            (set! duration duration-num)]
       #:args []
       (values concurrency duration))))
  (attack! concurrency duration))
