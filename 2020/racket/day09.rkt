#lang typed/racket

(module+ test
  (require typed/rackunit)
  )

(: find-add-nums : (Listof Integer) Integer -> Boolean)
(define (find-add-nums preamble h)
  (let* ([combs : (Listof (Listof Integer)) (combinations preamble 2)]
         [summer : (-> (Listof Integer) Integer)
                 (λ ([x : (Listof Integer)])  : Integer (apply + x))]
         [sums : (Listof Integer) (map summer combs)])
    (not (false? (member h sums)))))

(module+ test
  (check-true (find-add-nums '(35 20 15 25 47) 40))
  (check-false (find-add-nums '(35 20 15 25 47) 400))
  )

(: solve-inner : Integer (Listof Integer) (Listof Integer) -> (Option Integer))
(define (solve-inner i preamble lst)
  (printf "solve-inner, operating on ith: ~v\n" i)
  (match lst
    ([cons h t]
     (if (find-add-nums preamble h)
         (solve-inner (add1 i) (append (rest preamble) (list h)) t)
         h))
    (else
     (error "solve-inner: lst is empty, which should never happen"))))

(: solve : Integer (Listof Integer) -> (Option Integer))
(define (solve preamble-length nums)
  (let-values ([(preamble lst) (split-at nums preamble-length)]
               )
    ;; (printf "~v, ~v\n" preamble lst)
    (solve-inner preamble-length preamble lst)
    )
  )

(: unsafe-string->integer : String -> Integer)
(define (unsafe-string->integer str)
  (assert (string->number str) exact-integer?))

(: main (-> Void))
(define (main)
  (let* (
         [preamble-length : Integer 25]
         [in (open-input-file "day09-input")]
         ;; [preamble-length : Integer 5]
         ;; [in (open-input-file "day09-input-example")]
         [input-str : String (port->string in #:close? #t)]
         [lines : (Listof String) (string-split input-str "\n")]
         [nums : (Listof Integer) (map unsafe-string->integer lines)]
         [answer : (Option Integer) (solve preamble-length nums)]
         ;; [lines (string-split input-str "\n")]
         ;; [instructions
         ;;  (map (λ([x : String]) : (List String String)
         ;;         (assert (string-split x " ") 2list?))
         ;;       lines)]
         ;; [ops : (Listof Op) (map to-ops instructions)]
         ;; [final-res : (U False Integer) (interpret ops)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "lines: ~v\n" lines)
    ;; (printf "nums: ~v\n" nums)
    (printf "answer: ~a\n" answer)
    ;; (printf "final-res: ~a\n" final-res)
    (void)
    )
  )

(main)
