
#lang typed/racket

;; (struct pt ([x : Real] [y : Real]))

;; (define-type Tree (U leaf node))
;; (struct leaf ([val : Number]))
;; (struct node ([left : Tree] [right : Tree]))

(define-type Op (U nop acc jmp))
(struct nop ([val : Integer]))
(struct acc ([val : Integer]))
(struct jmp ([val : Integer]))

(: to-ops (-> (Listof String) Op))
(define (to-ops raw-ops)
  ;; (cond
  ;;   [(
  (match raw-ops
    [(list "nop" val)
     (nop (assert (string->number val) exact-integer?))]
    [(list "acc" val)
     (acc (assert (string->number val) exact-integer?))]
    [(list "jmp" val)
     (jmp (assert (string->number val) exact-integer?))]
    )
  )

(: main (-> Null))
(define (main)
  (let* (
         ;; [in (open-input-file "day08-input")]
         [in (open-input-file "day08-input-example")]
         [input-str (port->string in #:close? #t)]
         [lines (string-split input-str "\n")]
         [instructions (map (λ([x : String])(string-split x " ")) lines)]
         [ops (map to-ops instructions)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "lines: ~v\n" lines)
    (printf "instructions: ~v\n" instructions)
    (printf "ops: ~v\n" ops)
    '()
    )
  )

(main)
