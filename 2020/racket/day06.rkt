#lang racket

(module+ test
  (require rackunit)
  )


;; (define/contract (calc-group-amount group)
;;   (-> string? number?)
;;   (set-count (list->set (string->list (string-replace group "\n" ""))))
;;   )

(define/contract (calc-group-amount group)
  ;; (-> string? number?)
  (-> string? any/c)
  (let* ([individuals (string-split group "\n")] ; listof string?
         ;; listof (setof char?)
         [individual-sets (map (λ(indiv) (list->set (string->list indiv))) individuals)]
         ;; setof char?
         [intersection (apply set-intersect individual-sets)]
         )
    (set-count intersection)
    ;; intersection
    )
  )


(module+ test
  (check-equal? (calc-group-amount "abc") 3)
  ;; (check-equal? (calc-next-col 3 5 0) 3)
  )

(define (main)
  (let* (
         [in (open-input-file "day06-input")]
         ;; [in (open-input-file "day06-input-example")]
         [input-str (port->string in #:close? #t)]
         [groups (string-split input-str "\n\n")]
         [group-amount (map calc-group-amount groups)]
         [group-sum (for/sum ((g group-amount)) g)]
         ;; [seat-ids (map get-seat-id split-input)]
         ;; [max-seat-id (argmax identity seat-ids)]
         ;; [missing-seat-ids (get-missing-seat-ids seat-ids max-seat-id)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "groups: ~v\n" groups)
    ;; (printf "group-amount: ~v\n" group-amount)
    (printf "group-sum: ~v\n" group-sum)
    '()
    )
  )

;; (module+ main
(main)
;; )
