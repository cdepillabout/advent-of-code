
#lang racket

(module+ test
  (require rackunit)
  )

(define/contract (is-valid-str-part-2 letter min-val max-val str)
  (-> char? number? number? string? boolean?)
  (let ([letter-at-min (string-ref str (- min-val 1))]
        [letter-at-max (string-ref str (- max-val 1))]
        )
    (xor (eq? letter-at-min letter)
         (eq? letter-at-max letter)
         )
    ))

(module+ test
  (check-true (is-valid-str-part-2 #\a 1 3 "abcde"))
  (check-false (is-valid-str-part-2 #\b 1 3 "cdefg"))
  (check-false (is-valid-str-part-2 #\c 2 9 "ccccccccc"))
  ;; (check-equal? (split-range "10-22") '(10 . 22))
  )

(define/contract (is-valid-str-part-1 letter min-val max-val str)
  (-> char? number? number? string? boolean?)
  (let ([total-count-of-letter
         (count
          (λ(x) (eq? x letter))
          (string->list str))
         ])
    (<= min-val total-count-of-letter max-val)
  ))

(module+ test
  ;; (check-equal? (split-range "1-3") '(1 . 3))
  ;; (check-equal? (split-range "10-22") '(10 . 22))
  )

(define/contract (split-range str)
  (-> string? (cons/c number? number?))
  (match (string-split str "-")
    [(list min-val max-val)
     (cons (string->number min-val) (string->number max-val))])
  )

(module+ test
  (check-equal? (split-range "1-3") '(1 . 3))
  (check-equal? (split-range "10-22") '(10 . 22))
  )

(define/contract (split-letter str)
  (-> string? char?)
  (car (string->list (car (string-split str ":"))))
  )

(module+ test
  (check-equal? (split-letter "a:") #\a)
  (check-equal? (split-letter "z:") #\z)
  )

(define/contract (is-valid input)
  (-> string? boolean?)
  (match (string-split input " ")
    [(list
      (app split-range (cons min-val max-val))
      (app split-letter letter)
      str
      )
     (is-valid-str-part-2 letter min-val max-val str)
     ]
    ))

(module+ test
  (check-true (is-valid "1-3 a: abcde"))
  (check-false (is-valid "1-3 b: cdefg") "1-3 b: cdefg")
  (check-false (is-valid "2-9 c: ccccccccc"))
  )

(define (main)
  (let* (
         [in (open-input-file "day02-input")]
         ;; [in (open-input-file "day02-input-example")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str "\n")]
         [valids (map is-valid split-input)]
         [valids-count (count identity valids)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "valids: ~v\n" valids)
    (printf "valids-count: ~v\n" valids-count)
    )
  )

;; (module+ main
(main)
;; )
