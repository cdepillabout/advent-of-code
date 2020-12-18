
#lang typed/racket

(define-type Op (U nop acc jmp))
(struct nop ([val : Integer]))
(struct acc ([val : Integer]))
(struct jmp ([val : Integer]))

(: to-ops (-> (List String String) Op))
(define (to-ops raw-ops)
  ;; (cond
  ;;   [(
  (let* ([op-raw (car raw-ops)]
         [num-raw (cadr raw-ops)]
         [num (string->number num-raw)]
         )
    (if (exact-integer? num)
        (match op-raw
          ["nop" (nop num)]
          ["acc" (acc num)]
          ["jmp" (jmp num)]
          )
        (error "to-ops, num ~v on op ~v not exact integer\n" num op-raw))))

(: 2list? (All [A] (-> (Listof A) Boolean : (List A A))))
(define (2list? lst)
  (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst))))

;; TODO: How to write a default display function for Op???

(: main (-> Null))
(define (main)
  (let* (
         ;; [in (open-input-file "day08-input")]
         [in (open-input-file "day08-input-example")]
         [input-str (port->string in #:close? #t)]
         [lines (string-split input-str "\n")]
         [instructions
          (map (λ([x : String]) : (List String String)
                 (assert (string-split x " ") 2list?))
               lines)]
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
