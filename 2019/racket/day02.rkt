
#lang racket

(define (interp' nums i)
  (let ([op (list-ref nums i)])
    (match
      [(

(define (interp nums)
  (interp' nums 0)
  )

(define (main)
  (let* ([in (open-input-file "day02-input")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str ",")]
         [nums (map string->number split-input)]
         )
    (printf "~a\n" nums)
    )
  )

(main)
