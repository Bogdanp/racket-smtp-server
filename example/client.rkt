#lang racket/base

(require net/head
         net/smtp
         openssl
         racket/cmdline)

(define-values (subject sender recipients)
  (let ([the-subject "Hello!"])
    (command-line
     #:once-each
     [("--subject") subject "the e-mail subject line (default: 'Hello!')" (set! the-subject subject)]
     #:args [sender . recipients]
     (values the-subject sender recipients))))

(define lines
  (for/list ([line (in-lines (current-input-port))])
    line))

(smtp-send-message
 #:port-no 8675
 #:tls-encode ports->ssl-ports
 "127.0.0.1"
 sender
 recipients
 (insert-field
  "To" (assemble-address-field recipients)
  (insert-field "Subject" subject empty-header))
 lines)
