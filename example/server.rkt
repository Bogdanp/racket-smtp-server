#lang racket/base

(require net/smtp-server
         openssl)

(define ssl-context
  (ssl-make-server-context
   #:private-key `(pem "example.key")
   #:certificate-chain "example.crt"))

(define stop
  (start-smtp-server
   #:host "127.0.0.1"
   #:port 8675
   #:tls-encode (λ (in out
                       #:mode mode
                       #:encrypt protocol
                       #:close-original? close?)
                  (ports->ssl-ports
                   in out
                   #:mode mode
                   #:context ssl-context
                   #:encrypt protocol
                   #:close-original? close?))
   println))

(with-handlers ([exn:break? (λ (_) (stop))])
  (sync never-evt))
