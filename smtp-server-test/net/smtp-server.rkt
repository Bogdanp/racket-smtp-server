#lang racket/base

(require net/smtp
         net/smtp-server
         (submod net/smtp-server private)
         openssl
         rackcheck
         racket/runtime-path
         rackunit)

(define-runtime-path key-path  "smtp-server-example.key")
(define-runtime-path cert-path "smtp-server-example.crt")

(define (gen:smtp-line max-len)
  (gen:let ([content (gen:bytes #:max-length (- max-len 2))])
    (bytes-append (regexp-replace* #rx#"\r\n" content #"\r\x00") #"\r\n")))

(define all-tests
  (test-suite
   "smtp-server"

   (test-suite
    "reading"

    (test-suite
     "read-smtp-line!"

     (check-property
      (property ([max-len (gen:integer-in 2 100)]
                 [buf-len (gen:choice
                           (gen:const 1)
                           (gen:integer-in 1 128))]
                 [line (gen:smtp-line max-len)])
        (define line-buf (make-bytes max-len))
        (define scratch-buf (make-bytes buf-len))
        (define line-len
          (read-smtp-line! line-buf (open-input-bytes line) scratch-buf))
        (check-equal? line (subbytes line-buf 0 line-len)))))

    (test-suite
     "discard-smtp-line"

     (check-property
      (property ([max-len (gen:integer-in 2 100)]
                 [buf-len (gen:choice
                           (gen:const 1)
                           (gen:integer-in 1 128))]
                 [line (gen:smtp-line max-len)])
        (define scratch-buf (make-bytes buf-len))
        (define discarded-len
          (discard-smtp-line (open-input-bytes line) scratch-buf))
        (check-equal? (bytes-length line) discarded-len)))))

   (let ([stop #f] [envelopes null])
     (test-suite
      "end-to-end"

      #:before
      (lambda ()
        (define ssl-context
          (ssl-make-server-context
           #:private-key `(pem ,key-path)
           #:certificate-chain cert-path))
        (define stop-server
          (start-smtp-server
           #:port 10025
           #:tls-encode
           (lambda (in out #:mode mode #:encrypt protocol #:close-original? close?)
             (ports->ssl-ports
              in out
              #:mode mode
              #:context ssl-context
              #:encrypt protocol
              #:close-original? close?))
           (lambda (envelope)
             (set! envelopes (cons envelope envelopes)))))
        (set! stop stop-server))

      #:after
      (lambda ()
        (and stop (stop)))

      (test-case "receiving an e-mail in plain text"
        (smtp-send-message
         #:port-no 10025
         "127.0.0.1"
         "bogdan@defn.io"
         '("bogdan@example.com")
         "Subject: hi\r\n"
         (list "Hello!"))
        (check-equal?
         envelopes
         (list
          (envelope
           #"bogdan@defn.io"
           '(#"bogdan@example.com")
           #"Subject: hi\r\nHello!\r\n"))))

      (test-case "receiving an e-mail via STARTTLS"
        (set! envelopes null)
        (smtp-send-message
         #:port-no 10025
         #:tls-encode
         (lambda (in out #:mode mode #:encrypt protocol #:close-original? close?)
           (ports->ssl-ports
            in out
            #:mode mode
            #:context (ssl-make-client-context)
            #:encrypt protocol
            #:close-original? close?))
         "127.0.0.1"
         "bogdan@defn.io"
         '("bogdan@example.com" "paul@example.com")
         "Subject: hi\r\n"
         (list "Hello!"))
        (check-equal?
         envelopes
         (list
          (envelope
           #"bogdan@defn.io"
           '(#"paul@example.com" #"bogdan@example.com")
           #"Subject: hi\r\nHello!\r\n"))))))))


(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
