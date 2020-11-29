
#lang racket

(define (find-fuel mass)
  (- (quotient mass 3) 2))

(define (main)
  (let* ([in (open-input-file "day01-input")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str)]
         [nums (map string->number split-input)]
         [fuels (map find-fuel nums)]
         [sum (apply + fuels)]
         )
    (print sum)
    )
  )

(main)
