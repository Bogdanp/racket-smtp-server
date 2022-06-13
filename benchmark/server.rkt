#lang racket/base

(require net/smtp-server)

(define stop
  (start-smtp-server
   #:host "127.0.0.1"
   #:port 8675
   void))

(with-handlers ([exn:break? (λ (_) (stop))])
  (sync never-evt))
