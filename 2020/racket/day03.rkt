
#lang racket

(module+ test
  (require rackunit)
  )

(define/contract (calc-next-col max-cols curr-col)
  (-> number? number? number?)
  (modulo (+ curr-col 3) max-cols)
  )

(module+ test
  (check-equal? (calc-next-col 5 3) 1)
  (check-equal? (calc-next-col 5 0) 3)
  )

(define/contract (is-tree-here? tree-line-lists curr-row curr-col)
  (-> (listof (listof boolean?)) number? number? boolean?)
  (list-ref (list-ref tree-line-lists curr-row) curr-col)
  )

(module+ test
  (check-true (is-tree-here? '((#f #f #t)
                               (#f #f #t))
                             1 2))
  )

(define/contract (count-trees-loop tree-line-lists max-rows max-cols curr-row curr-col num-trees-accum)
  (-> (listof (listof boolean?)) number? number? number? number? number? number?)
  (when (>= curr-col max-cols) (error "violation curr-col greater than max-cols in count-trees-loop"))
  (if (eq? curr-row max-rows)
      num-trees-accum
      (let ([is-tree-here (is-tree-here? tree-line-lists curr-row curr-col)]
            [next-col (calc-next-col max-cols curr-col)]
            )
        (count-trees-loop tree-line-lists
                          max-rows
                          max-cols
                          (+ 1 curr-row)
                          next-col
                          (if is-tree-here (+ 1 num-trees-accum) num-trees-accum)
                          )
        )
      )
  )

(define/contract (count-trees tree-line-lists)
  (-> (listof (listof boolean?)) number?)
  (let ([max-rows (length tree-line-lists)]
        [max-cols (length (car tree-line-lists))]
        )
    ;; (printf "num-rows: ~v, num-cols: ~v\n" num-rows num-cols)
    (count-trees-loop tree-line-lists max-rows max-cols 0 0 0)
    )
  )

(module+ test
  ;; (check-equal? (split-letter "a:") #\a)
  ;; (check-equal? (split-letter "z:") #\z)
  )

(define/contract (to-tree-line str)
  (-> string? (listof boolean?))
  (map (λ(x)(eq? x #\#)) (string->list str))
  )

(module+ test
  (check-equal? (to-tree-line "#..#..") '(#t #f #f #t #f #f))
  ;; (check-true (is-valid "1-3 a: abcde"))
  ;; (check-false (is-valid "1-3 b: cdefg") "1-3 b: cdefg")
  )

(define (main)
  (let* (
         [in (open-input-file "day03-input")]
         ;; [in (open-input-file "day03-input-example")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str "\n")]
         [tree-line-lists (map to-tree-line split-input)]
         [number-collisions (count-trees tree-line-lists)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "tree-line-lists: ~v\n" tree-line-lists)
    (printf "number-collisions: ~v\n" number-collisions)
    )
  )

;; (module+ main
(main)
;; )
