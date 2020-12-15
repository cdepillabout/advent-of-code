#lang racket
(require data/applicative)
(require data/either)
(require data/functor)
(require data/monad)
(require megaparsack megaparsack/text)

(module+ test
  (require rackunit)
  )

;; light red bags contain 1 bright white bag, 2 muted yellow bags.
;; bright white bags contain 1 shiny gold bag.
;; faded blue bags contain no other bags.

(define single-color/p
  (map list->string (many+/p letter/p)))

(module+ test
  (check-equal? (parse-string single-color/p "hello tomorrow") (success "hello"))
  )

(define/contract color/p
  (parser/c char? string?)
  (do
      [desc <- single-color/p]
      space/p
      [col <- single-color/p]
      (pure (string-append desc " " col))
      )
  )

(module+ test
  (check-equal? (parse-string color/p "hello tomorrow")
                (success "hello tomorrow"))
  )

(define/contract contained-bag/p
  (parser/c char? list?)
  (do
      [num-bags <- integer/p]
      space/p
    [col <- color/p]
    space/p
    (or/p (try/p (string/p "bags"))
          (string/p "bag"))
    (pure (list col num-bags)))
  )

(module+ test
  (check-equal? (parse-string
                 contained-bag/p
                 "1 bright white bag"
                 )
                (success '("bright white" 1))
                )
  )


(define/contract contained-bags/p
  (parser/c char? any/c)
  (do [contained <- (or/p (many+/p contained-bag/p #:sep (string/p ", "))
                         (map (const '()) (string/p "no other bags")))]
      (char/p #\.)
      (pure contained)
    )
  )

(module+ test
  (check-equal? (parse-string
                 contained-bags/p
                 "1 bright white bag, 2 muted yellow bags."
                 )
                (success '(("bright white" 1)
                           ("muted yellow" 2)
                           )
                         )
                )
  (check-equal? (parse-string
                 contained-bags/p
                 "no other bags."
                 )
                (success '())
                )
  )

(define/contract contains/p
  (parser/c char? list?)
  (do
      [color <- color/p]
      (string/p " bags contain ")
      [bags <- contained-bags/p]
      (pure (list color bags))
    )
  )

(module+ test
  (check-equal? (parse-string
                 contains/p
                 "light red bags contain 1 bright white bag, 2 muted yellow bags."
                 )
                (success '("light red"
                           (("bright white" 1)
                            ("muted yellow" 2)))))
  (check-equal? (parse-string
                 contains/p
                 "bright white bags contain 1 shiny gold bag."
                 )
                (success '("bright white"
                           (("shiny gold" 1)))))
  (check-equal? (parse-string
                 contains/p
                 "faded blue bags contain no other bags."
                 )
                (success '("faded blue" ())))
  )

(define all-input/p
  (many+/p contains/p #:sep (string/p "\n")))

(define (main)
  (let* (
         ;; [in (open-input-file "day07-input")]
         [in (open-input-file "day07-input-example")]
         [input-str (port->string in #:close? #t)]
         [input-lines (parse-result! (parse-string all-input/p input-str))]
         ;; [group-amount (map calc-group-amount groups)]
         ;; [group-sum (for/sum ((g group-amount)) g)]
         ;; [seat-ids (map get-seat-id split-input)]
         ;; [max-seat-id (argmax identity seat-ids)]
         ;; [missing-seat-ids (get-missing-seat-ids seat-ids max-seat-id)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    (printf "input-lines: ~v\n" input-lines)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "groups: ~v\n" groups)
    ;; (printf "group-amount: ~v\n" group-amount)
    ;; (printf "group-sum: ~v\n" group-sum)
    '()
    )
  )

;; (module+ main
(main)
;; )
