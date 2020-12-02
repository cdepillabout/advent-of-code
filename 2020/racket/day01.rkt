
#lang racket

(define (create-couples lst)
  (combinations lst 3)
  )

(define (sum-couple lst)
  (match lst
    [(list x y z) (list (+ x y z) x y z)]
    [my-lst (error (format "Not three elements in list: ~a\n" my-lst))]
  ))

(define (is-2020 lst)
  (match lst
    [(list 2020 x y z) #t]
    [_ #f]
    ))

(define (main)
  (let* ([in (open-input-file "day01-input")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str)]
         [nums (map string->number split-input)]
         [couples (create-couples nums)]
         [couples-sum (map sum-couple couples)]
         [filtered-couples (filter is-2020 couples-sum)]
         )
    ;; (printf "~a\n" (first couples-sum))
    ;; (printf "~a\n" (first couples))
    ;; (printf "~a\n" (length nums))
    ;; (printf "~a\n" (length couples-sum))
    (printf "~a\n" filtered-couples)
    )
  )

(main)
