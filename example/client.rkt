#lang racket/base

(require net/smtp
         openssl
         racket/cmdline)

(define-values (sender recipients)
  (command-line
   #:args [sender . recipients]
   (values sender recipients)))

(define lines
  (for/list ([line (in-lines (current-input-port))])
    line))

(smtp-send-message
 #:port-no 8675
 #:tls-encode ports->ssl-ports
 "127.0.0.1" sender recipients "" lines)
