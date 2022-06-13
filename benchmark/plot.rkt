#lang racket/base

(require plot
         racket/cmdline
         racket/match
         racket/string)

(define filenames
  (command-line
   #:args filenames
   filenames))

(define (read-log filename)
  (call-with-input-file filename
    (lambda (in)
      (filter values (for/list ([line (in-lines in)])
                       (match line
                         [(regexp "GC: 0:.* @ ([^K]+)K.*; free .* @ (.+)" (list _ ks ts))
                          (list (string->number ts)
                                (round (/ (string->number (string-replace ks "," "")) 1024)))]
                         [_
                          #f]))))))

(parameterize ([plot-new-window? #t]
               [plot-width 800]
               [plot-height 600]
               [plot-x-label "Milliseconds"]
               [plot-y-label "MiB"])
  (plot (for/list ([(filename idx) (in-indexed (in-list filenames))])
          (lines
           #:label filename
           #:color idx
           (read-log filename)))))
