
#lang typed/racket

(define-type Op (U nop acc jmp))
(struct nop ([val : Integer]) #:transparent)
(struct acc ([val : Integer]) #:transparent)
(struct jmp ([val : Integer]) #:transparent)

(: to-ops (-> (List String String) Op))
(define (to-ops raw-ops)
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

(: interp (-> (Listof Op) Integer Integer (Setof Integer) Integer))
(define (interp ops pc accum visited-ops)
  (let ([curr-op : Op (list-ref ops pc)]
        [new-visited-ops : (Setof Integer) (set-add visited-ops pc)]
        )
    (if (set-member? visited-ops pc)
        accum
        (match curr-op
          [(nop _) (interp ops (add1 pc) accum new-visited-ops)]
          [(acc amt) (interp ops (add1 pc) (+ accum amt) new-visited-ops)]
          [(jmp next-op) (interp ops (+ pc next-op) accum new-visited-ops)]
          ))))

(: interpret (-> (Listof Op) Integer))
(define (interpret ops)
  (interp ops 0 0 (set)))

(: main (-> Null))
(define (main)
  (let* (
         [in (open-input-file "day08-input")]
         ;; [in (open-input-file "day08-input-example")]
         [input-str (port->string in #:close? #t)]
         [lines (string-split input-str "\n")]
         [instructions
          (map (λ([x : String]) : (List String String)
                 (assert (string-split x " ") 2list?))
               lines)]
         [ops : (Listof Op) (map to-ops instructions)]
         [final-res : Integer (interpret ops)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "lines: ~v\n" lines)
    (printf "instructions: ~v\n" instructions)
    (printf "ops: ~a\n" ops)
    (printf "final-res: ~a\n" final-res)
    '()
    )
  )

(main)
