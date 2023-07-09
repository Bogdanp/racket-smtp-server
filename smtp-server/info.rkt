#lang info

(define license 'BSD-3-Clause)
(define version "0.1")
(define collection "net")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define scribblings '(("scribblings/smtp-server.scrbl" () (net-library))))
