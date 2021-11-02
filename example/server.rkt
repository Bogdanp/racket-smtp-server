#lang racket/base

(require net/mime
         net/smtp-server
         openssl
         racket/port
         racket/pretty)

(define (pp-message m)
  (define (help v)
    (cond
      [(message? v)
       `((fields . ,(message-fields v))
         (entity . ,(help (message-entity v))))]

      [(entity? v)
       `((type . ,(entity-type v))
         (subtype . ,(entity-subtype v))
         (fields . ,(entity-fields v))
         (parts . ,(map help (entity-parts v)))
         (body . ,(and
                   (not (null? (entity-body v)))
                   (call-with-output-bytes (entity-body v)))))]

      [else
       (raise-argument-error 'pp-message "(or/c message? entity?)" v)]))
  (pretty-print (help m)))

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
   (compose1 pp-message mime-analyze envelope-data)))

(with-handlers ([exn:break? (λ (_) (stop))])
  (sync never-evt))
