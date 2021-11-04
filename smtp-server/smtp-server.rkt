#lang racket/base

(require racket/contract
         racket/os
         racket/string
         racket/tcp)

;; params ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [current-smtp-hostname (parameter/c non-empty-string?)]
  [current-smtp-max-line-length (parameter/c exact-nonnegative-integer?)]
  [current-smtp-max-envelope-length (parameter/c exact-nonnegative-integer?)]))

(define current-smtp-hostname
  (make-parameter (gethostname)))

(define current-smtp-max-line-length
  (make-parameter 1024))

(define current-smtp-max-envelope-length
  (make-parameter (* 10 1024 1024)))


;; envelope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  (struct envelope
    ([sender bytes?]
     [recipients (listof bytes?)]
     [data bytes?]))))

(struct envelope (sender recipients data)
  #:transparent)

(define (make-envelope sender)
  (envelope sender null #""))

(define (add-envelope-recipient e rcpt)
  (struct-copy envelope e [recipients (cons rcpt (envelope-recipients e))]))

(define (add-envelope-data e data)
  (struct-copy envelope e [data data]))

(define (envelope-length e)
  (+ (bytes-length (envelope-data e))
     (bytes-length (envelope-sender e))
     (for/sum ([r (in-list (envelope-recipients e))])
       (bytes-length r))))

(define (envelope-too-long? e)
  (> (envelope-length e)
     (current-smtp-max-envelope-length)))


;; server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 tls-encode-proc/c
 (contract-out
  [start-smtp-server (->* ((-> envelope? void?))
                          (#:host string?
                           #:port (integer-in 0 65535)
                           #:tls-encode (or/c #f tls-encode-proc/c))
                          (-> void?))]))

(define-logger smtp-server)

(define tls-encode-proc/c
  (-> input-port?
      output-port?
      #:mode 'accept
      #:encrypt 'tls
      #:close-original? #t
      (values input-port? output-port?)))

(define (start-smtp-server handler
                           #:host [host "127.0.0.1"]
                           #:port [port 25]
                           #:tls-encode [tls-encode #f])
  (define cust (make-custodian))
  (define stop-ch (make-channel))
  (define listener
    (tcp-listen port 128 #t host))
  (define server-thd
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (let loop ()
           (sync
            (handle-evt stop-ch void)
            (handle-evt
             listener
             (lambda (_)
               (define-values (in out)
                 (tcp-accept listener))
               (thread (λ () (client-loop in out handler tls-encode)))
               (loop)))))))))
  (lambda ()
    (channel-put stop-ch #t)
    (thread-wait server-thd)
    (custodian-shutdown-all cust)
    (tcp-close listener)))

(define (client-loop in out handler [tls-encode #f])
  (define line-buf (make-bytes (current-smtp-max-line-length)))
  (define scratch-buf (make-bytes (current-smtp-max-line-length)))
  (let connection-loop ([in in]
                        [out out]
                        [start? #t])
    (define (rep- status message)
      (fprintf out "~a-~a\r\n" status message))
    (define (rep status [message "OK"])
      (fprintf out "~a ~a\r\n" status message)
      (flush-output out))
    (when start?
      (rep 220 (current-smtp-hostname)))
    (with-handlers ([exn:fail?
                     (λ (e)
                       (log-smtp-server-warning "unhandled error: ~a" (exn-message e)))])
      (let loop ([envelope #f])
        (define line-len (read-smtp-line! line-buf in scratch-buf))
        (case (and line-len (parse-command line-buf line-len))
          [(#f)
           (discard-smtp-line in scratch-buf)
           (rep 500 "line too long")
           (loop #f)]

          [(#"HELO")
           (rep 250)
           (loop #f)]

          [(#"EHLO")
           (rep- 250 (current-smtp-hostname))
           (rep- 250 "8BITMIME")
           (rep- 250 (format "SIZE ~a" (current-smtp-max-envelope-length)))
           (when tls-encode
             (rep- 250 "STARTTLS"))
           (rep 250)
           (loop #f)]

          [(#"RSET")
           (rep 250)
           (loop #f)]

          [(#"NOOP")
           (rep 250)
           (loop envelope)]

          [(#"STARTTLS")
           (with-handlers ([exn:fail? (λ (_)
                                        (log-smtp-server-warning "TLS handshake failed")
                                        (rep 500 "protocol error")
                                        (loop #f))])
             (rep 220)
             (log-smtp-server-debug "initiating TLS handshake")
             (define-values (ssl-in ssl-out)
               (tls-encode in out #:mode 'accept #:encrypt 'tls #:close-original? #t))
             (log-smtp-server-debug "TLS connection initiatied")
             (connection-loop ssl-in ssl-out #f))]

          [(#"MAIL")
           ;; Potential improvements:
           ;;   * Handling of 7BIT or 8BITMIME params from RFC1652
           ;;   * Handling of SIZE= param from RFC1870
           (cond
             [envelope
              (rep 503 "nested MAIL command")
              (loop envelope)]

             [(regexp-match #rx#"^(?i:(mail from:<([^>]+)>))" line-buf 0 line-len)
              => (λ (matches)
                   (define new-envelope
                     (make-envelope (caddr matches)))
                   (cond
                     [(envelope-too-long? new-envelope)
                      (rep 552 "message exceeds fixed message maximum size")
                      (loop #f)]

                     [else
                      (rep 250)
                      (loop (make-envelope (caddr matches)))]))]

             [else
              (rep 501 "syntax: MAIL FROM:<ADDRESS>")
              (loop #f)])]

          [(#"RCPT")
           (cond
             [(and envelope (regexp-match #rx#"^(?i:(rcpt to:<([^>]+)>))" line-buf 0 line-len))
              => (λ (matches)
                   (define new-envelope
                     (add-envelope-recipient envelope (caddr matches)))
                   (cond
                     [(envelope-too-long? new-envelope)
                      (rep 552 "message exceeds fixed message maximum size")
                      (loop envelope)]

                     [else
                      (rep 250)
                      (loop new-envelope)]))]

             [envelope
              (rep 501 "syntax: RCPT TO:<ADDRESS>")
              (loop envelope)]

             [else
              (rep 503 "need MAIL command")
              (loop #f)])]

          [(#"DATA")
           (cond
             [(and envelope (null? (envelope-recipients envelope)))
              (rep 503 "need recipients")
              (loop envelope)]

             [envelope
              (rep 354 "end data with <CRLF>.<CRLF>")
              (define max-len
                (- (current-smtp-max-envelope-length)
                   (envelope-length envelope)))
              (define data
                (read-mail-data line-buf in scratch-buf max-len))
              (cond
                [data
                 (with-handlers ([exn:fail?
                                  (λ (e)
                                    (log-smtp-server-warning "unexpected handler error: ~a" (exn-message e))
                                    (rep 554 "internal error")
                                    (loop #f))])
                   (handler (add-envelope-data envelope data))
                   (rep 250)
                   (loop #f))]

                [else
                 (rep 552 "message exceeds fixed message maximum size")
                 (loop envelope)])]

             [else
              (rep 503 "need MAIL command")
              (loop #f)])]

          [(#"QUIT")
           (rep 221 "goodbye")]

          [else
           (rep 502 "command not recognized")
           (loop envelope)])))
    (close-output-port out)
    (close-input-port in)))

(define (parse-command line [len (bytes-length line)])
  (define end
    (or (find-sp   line len)
        (find-crlf line len)
        len))
  (define bs (subbytes line 0 end))
  (begin0 bs
    (bytes-upcase! bs)))

(define (bytes-upcase! bs)
  (for ([i (in-naturals)]
        [b (in-bytes bs)]
        #:when (and (>= b 97)
                    (<= b 122)))
    (bytes-set! bs i (- b 32))))


;; reading ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The reading functions each try to read data with minimal allocations
;; and buffering. They assume that the other end is adversarial.

(module+ private
  (provide
   read-mail-data
   read-smtp-line!
   discard-smtp-line))

;; Reads mail DATA line-by-line until a #".\x0D\x0A" line.  If the
;; length of the data exceeds `max-len' or if any of the lines exceeds
;; the current max line length, it discards all the lines up to and
;; including terminator line and returns `#f'.
(define (read-mail-data line-buf in buf max-len)
  (let loop ([data-len 0]
             [lines null]
             [failed? #f])
    (define line-len
      (read-smtp-line! line-buf in buf))
    (define next-len
      (and line-len (+ data-len line-len)))
    (cond
      [(not line-len)
       (loop (+ data-len (discard-smtp-line in buf)) null #t)]

      [(zero? line-len)
       #f]

      [(terminator? line-buf line-len)
       (and (not failed?)
            (apply bytes-append (reverse lines)))]

      [(<= next-len max-len)
       (loop next-len (cons (subbytes line-buf 0 line-len) lines) failed?)]

      [else
       (loop next-len null #t)])))

;; Reads a CRLF-terminated line from `in' into `bs'.  Returns `#f' if
;; the line is longer than `bs', and the number of bytes read
;; otherwise.
(define (read-smtp-line! bs in [buf (make-bytes 4096)])
  (define len (bytes-length bs))
  (let loop ([start 0] [pending-lf? #f])
    (define n-peeked
      (peek-bytes-avail! buf 0 #f in))
    (cond
      [(eof-object? n-peeked)
       start]

      [(and pending-lf? (lf? (bytes-ref buf 0)))
       (+ start (read-bytes! bs in start (add1 start)))]

      [(find-crlf buf n-peeked)
       => (λ (crlf-pos)
            (define end
              (+ start crlf-pos 2))
            (and (<= end len)
                 (+ start (read-bytes! bs in start end))))]

      [else
       (define end
         (+ start n-peeked))
       (and (<= end (sub1 len))
            (loop (+ start (read-bytes! bs in start end))
                  (cr? (bytes-ref buf (sub1 n-peeked)))))])))

;; Discards all data from `in' up to the next CRLF or EOF.  Returns
;; the number of discarded bytes.
(define (discard-smtp-line in [buf (make-bytes 4096)])
  (let loop ([n-discarded 0]
             [pending-lf? #f])
    (define n-peeked
      (peek-bytes-avail! buf 0 #f in))
    (cond
      [(eof-object? n-peeked)
       n-discarded]

      [(and pending-lf? (lf? (bytes-ref buf 0)))
       (+ n-discarded (read-bytes! buf in 0 1))]

      [(find-crlf buf n-peeked)
       => (λ (pos)
            (+ n-discarded (read-bytes! buf in 0 (+ pos 2))))]

      [else
       (define pending? (cr? (bytes-ref buf (sub1 n-peeked))))
       (loop (+ n-discarded (read-bytes! buf in 0 n-peeked)) pending?)])))

(define (find-sp bs [stop (bytes-length bs)])
  (for/first ([p (in-naturals)]
              [b (in-bytes bs 0 stop)]
              #:when (sp? b))
    p))

(define (find-crlf bs [stop (bytes-length bs)])
  (and (not (zero? stop))
       (for/first ([p (in-naturals)]
                   [b-cr (in-bytes bs 0 stop)]
                   [b-lf (in-bytes bs 1 stop)]
                   #:when (and (cr? b-cr)
                               (lf? b-lf)))
         p)))

(define (sp?  b) (= b #x20))
(define (cr?  b) (= b #x0D))
(define (lf?  b) (= b #x0A))
(define (dot? b) (= b #x2E))

(define (terminator? bs len)
  (and (= len 3)
       (dot? (bytes-ref bs 0))
       (cr?  (bytes-ref bs 1))
       (lf?  (bytes-ref bs 2))))
