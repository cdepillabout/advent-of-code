
#lang racket

(define (find-fuel mass)
  (- (quotient mass 3) 2))

(define (find-fuel-recursive mass)
  (let* ([the-fuel (find-fuel mass)])
    (if (<= the-fuel 0)
        '()
        (cons the-fuel (find-fuel-recursive the-fuel)))))

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

(define (sum lst) (foldl + 0 lst))

(define (main-part-2)
  (let* ([in (open-input-file "day01-input")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str)]
         [nums (map string->number split-input)]
         [fuels (map find-fuel-recursive nums)]
         ;; [sum (apply (λ (x) (apply + x)) fuels)]
         [fuels-sum (apply + (flatten fuels))]
         [fuels-sum2 (sum (map sum fuels))]
         )
    (printf "~a\n" fuels-sum)
    (printf "~a\n" fuels-sum2)
    )
  )

(main-part-2)
