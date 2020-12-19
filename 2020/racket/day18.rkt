#lang racket
(require data/applicative)
(require data/either)
(require (rename-in data/functor [map functor/map]))
(require (rename-in data/monad [do monad/do]))
(require megaparsack megaparsack/text)

(module+ test
  (require rackunit)
  )

(define (is-op? c) (or/c (char=? c #\+) (char=? c #\*)))

(define (op-and-expr/c) (list/c is-op? (expr/c)))

(define (expr-list/c) (list/c (expr/c) (listof (op-and-expr/c))))

;; (define (expr/c) (or/c integer? (expr-list/c)))
(define (expr/c) (or/c integer?))


;;;;;;;;;;;;;
;; Parsing ;;
;;;;;;;;;;;;;

(define/contract op/p (parser/c char? is-op?)
  (or/p (char/p #\+) (char/p #\*)))

(module+ test
  (check-equal? (parse-string op/p "+") (success #\+))
  (check-equal? (parse-string op/p "*") (success #\*))
  )

(define (op-and-expr/p)
  ;; (parser/c char? (op-and-expr/c))
  (monad/do
   (string/p " ")
   [op <- op/p]
   (string/p " ")
   [expr <- (expr/p)]
   (pure (list op expr))))

(define (expr/p)
  ;; (parser/c char? (expr/c))
  (or/p integer/p
        (monad/do
         [char/p #\(]
         [expr <- (expr-list/p)]
         [char/p #\)]
         (pure expr)
         )))

(module+ test
  (check-equal? (parse-string (expr/p) "7") (success 7))
  (check-equal? (parse-string (expr/p) "0") (success 0))
  (check-equal? (parse-string (expr/p) "(0)") (success '(0 ())))
  (check-equal? (parse-string (expr/p) "(0)\n") (success '(0 ())))
  (check-equal? (parse-string (expr/p) "(0 + 3)") (success '(0 ((#\+ 3)))))
  )

(define (expr-list/p)
  ;; (parser/c char? (expr-list/c))
  (monad/do
   [first-expr <- (expr/p)]
   [op-and-exprs <- (many/p (op-and-expr/p))]
   [pure (list first-expr op-and-exprs)]
   )
  )

(module+ test
  (check-equal?
   (parse-string (expr-list/p) "1 + 2 * 3 + 4 * 5 + 6")
   (success '(1 ((#\+ 2) (#\* 3) (#\+ 4) (#\* 5) (#\+ 6)))))
  (check-equal?
   (parse-string (expr-list/p) "1 + 2 * 3 + 4 * 5 + 6\n")
   (success '(1 ((#\+ 2) (#\* 3) (#\+ 4) (#\* 5) (#\+ 6)))))
  (check-equal?
   (parse-string (expr-list/p) "1 + (2 * 3) + (4 * (5 + 6))")
   (success '(1 ((#\+ (2 ((#\* 3)))) (#\+ (4 ((#\* (5 ((#\+ 6))))))))))
   )
  (check-equal?
   (parse-string (expr-list/p) "9 + (8 * (8 + 7 * 4)) * 6 + 5 + 4 * 9")
   (success
    '(9 ((#\+ (8 ((#\* (8 ((#\+ 7) (#\* 4)))))))
         (#\* 6)
         (#\+ 5)
         (#\+ 4)
         (#\* 9))))

   )
  )

(define parse-all/p
  ;; (parser/c char? (listof (expr-list/c)))
  (many+/p (expr-list/p) #:sep (string/p "\n"))
  )

(module+ test
  (check-equal? (parse-string parse-all/p "1") (success '((1 ()))))
  (check-equal? (parse-string parse-all/p "1\n1") (success '((1 ()) (1 ()))))
  (check-equal?
   (parse-string
    parse-all/p
    "1 + 2 * 3 + 4 * 5 + 6\n1 + (2 * 3) + (4 * (5 + 6))")
   (success
    '((1 ((#\+ 2) (#\* 3) (#\+ 4) (#\* 5) (#\+ 6)))
      (1 ((#\+ (2 ((#\* 3)))) (#\+ (4 ((#\* (5 ((#\+ 6))))))))))
    )
   )
  )

;;;;;;;;;;;;;;
;; Solution ;;
;;;;;;;;;;;;;;

(define (solve-expr expr)
  ;; (-> (expr/c) number?)
  (cond
    [(number? expr) expr]
    [else (solve-parsed expr)]))

(define (solve-num-and-next num ops-and-exprs)
  ;; (-> number? (listof (op-and-expr/c)))
  (cond
    [(null? ops-and-exprs) num]
    [else
     (let* ([first-op-and-expr (car ops-and-exprs)]
            [first-op (car first-op-and-expr)] ;; char?
            [first-expr (cadr first-op-and-expr)]
            [solved-first-expr (solve-expr first-expr)] ;; number?
            [remaining (cdr ops-and-exprs)] ;; (listof (op-and-expr/c))
            )
       (cond
         [(char=? first-op #\+)
          (solve-num-and-next (+ num solved-first-expr) remaining)]
         [(char=? first-op #\*)
          (* num (solve-num-and-next solved-first-expr remaining))]
         ))]))

(define (solve-parsed parsed)
  ;; (-> (expr-list/c) number?)
  '()
  (match parsed
    [(list expr next-op-and-exprs)
     (solve-num-and-next (solve-expr expr) next-op-and-exprs)
     ]
    )
  )

(define/contract (solve-line str)
  (-> string? number?)
  (let* ([parsed (parse-result! (parse-string (expr-list/p) str))]
         [solved (solve-parsed parsed)]
         )
    solved))

(module+ test
  (check-equal? (solve-line "1 + 2 * 3 + 4 * 5 + 6") 231)
  (check-equal? (solve-line "1 + (2 * 3) + (4 * (5 + 6))") 51)
  (check-equal? (solve-line "2 * 3 + (4 * 5)") 46)
  (check-equal? (solve-line "5 + (8 * 3 + 9 + 3 * 4 * 3)") 1445)
  (check-equal? (solve-line "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 669060)
  (check-equal? (solve-line "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 23340)
  ;; (check-equal? (solve-line "9 + (8 * (8 + 7 * 4)) * 6 + 5 + 4 * 9") 26487)

  )

(define/contract (parse-and-solve-all str)
  (-> string? (listof number?))
  (let* ([parsed (parse-result! (parse-string parse-all/p str))]
         [solved (map solve-parsed parsed)]
         )
    solved))

(module+ test
  ;; (check-equal?
  ;;  (parse-and-solve-all "1 + 2 * 3 + 4 * 5 + 6\n1 + (2 * 3) + (4 * (5 + 6))")
  ;;  '(71 51))
  )


(define (main)
  (let* (
         [in (open-input-file "day18-input")]
         [input-str (port->string in #:close? #t)]
         ;; [split-input (string-split input-str "\n")]
         [solutions (parse-and-solve-all input-str)]
         [sum (for/sum ([i solutions]) i)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    (printf "sum: ~v\n" sum)
    (void)
    )
  )

;; (module+ main
(main)
;; )
