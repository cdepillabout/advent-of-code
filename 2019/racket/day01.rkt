
#lang racket

(define (main)
  (let* ([in (open-input-file "day01-input")]
         [input-str (port->string in #:close? #t)]
         )
    (print input-str)
    )
  )

(main)
