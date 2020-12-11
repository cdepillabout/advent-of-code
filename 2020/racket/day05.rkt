
#lang racket

(module+ test
  (require rackunit)
  )


(module+ test
  ;; (check-equal? (calc-next-col 3 5 3) 1)
  ;; (check-equal? (calc-next-col 3 5 0) 3)
  )

(module+ test
  ;; (check-true (is-tree-here? '((#f #f #t)
  ;;                              (#f #f #t))
  ;;                            1 2))
  )


(define/contract (get-seat-id str)
  (-> string? number?)
  (let* ([replaces '(("F" "0") ("B" "1") ("R" "1") ("L" "0"))]
         [binary-str
          (foldl
           (λ(reps accum-str) (string-replace accum-str (car reps) (cadr reps)))
           str
           replaces
           )
          ])
    ;; (read-binary-str binary-str
    (read (open-input-string (string-append "#b" binary-str)))
    )
  )


(module+ test
  (check-equal? (get-seat-id "FBFBBFFRLR") 357)
  (check-equal? (get-seat-id "BFFFBBFRRR") 567)
  (check-equal? (get-seat-id "FFFBBBFRRR") 119)
  (check-equal? (get-seat-id "BBFFBBFRLL") 820)
  ;; (check-equal? (calc-next-col 3 5 0) 3)
  )

(define/contract (get-missing-seat-ids seat-ids max-seat-ids)
  (-> (listof number?) number? (listof number?))
  (let ([sorted-seat-ids (list->set (sort seat-ids <))]
        [all-range (list->set (range 0 max-seat-ids))]
        )
    (sort (set->list (set-subtract all-range sorted-seat-ids)) <)
    )
  )



(define (main)
  (let* (
         [in (open-input-file "day05-input")]
         ;; [in (open-input-file "day03-input-example")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str "\n")]
         [seat-ids (map get-seat-id split-input)]
         [max-seat-id (argmax identity seat-ids)]
         [missing-seat-ids (get-missing-seat-ids seat-ids max-seat-id)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "tree-line-lists: ~v\n" tree-line-lists)
    ;; (printf "number-collisions: ~v\n" number-collisions)
    ;; (printf "collisions-for-pattern: ~v\n" collisions-for-pattern)
    (printf "max-seat-id: ~v\n" max-seat-id)
    (printf "missing-seat-ids: ~v\n" missing-seat-ids)
    '()
    )
  )

;; (module+ main
(main)
;; )
