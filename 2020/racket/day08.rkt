
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

(: interp (-> (Listof Op)
              Index
              Integer
              Integer
              (Setof Integer)
              Boolean
              (U Integer False)))
(define (interp ops ops-len pc accum visited-ops should-try-switch)
  (cond
    [(set-member? visited-ops pc) #f]
    [(>= pc ops-len) accum]
    [else
     (let ([curr-op : Op (list-ref ops pc)]
           [new-visited-ops : (Setof Integer) (set-add visited-ops pc)]
           )
       (match curr-op
         [(acc amt)
          (interp ops ops-len (add1 pc) (+ accum amt)
                  new-visited-ops should-try-switch)]

         [(nop next-op)
          (if should-try-switch
              ;; If this is the first time through, try to switch to instead
              ;; treating this as a jump and run to the end.
              (let ([inner-res
                     (interp ops ops-len (+ pc next-op) accum new-visited-ops #f)])
                (if inner-res
                    ;; We got a result!  This was correct op to switch.  Return the
                    ;; result.
                    inner-res
                    ;; We didn't get a result.  This was not the
                    ;; correct op to switch and we looped.  Try
                    ;; running this nop normally and switching on the
                    ;; next one.
                    (interp ops ops-len (add1 pc) accum new-visited-ops #t)))

              ;; If we've already tried switching a previous op, then
              ;; don't try again, just run to the end, treating this
              ;; as a nop
              (interp ops ops-len (add1 pc) accum new-visited-ops #f))]

         [(jmp next-op)
          (if should-try-switch
              ;; If this is the first time through, try to switch to instead
              ;; treating this as a nop and run to the end.
              (let ([inner-res
                     (interp ops ops-len (add1 pc) accum new-visited-ops #f)])
                (if inner-res
                    ;; We got a result!  This was correct op to switch.  Return the
                    ;; result.
                    inner-res
                    ;; We didn't get a result.  This was not the
                    ;; correct op to switch and we looped.  Try
                    ;; running this jump and switching on the next
                    ;; one.
                    (interp ops ops-len (+ pc next-op) accum new-visited-ops #t)))

              ;; If we've already tried switching a previous op, then
              ;; don't try again, just run to the end, treating this
              ;; as a jmp
              (interp ops ops-len (+ pc next-op) accum new-visited-ops #f))]
         ))]))

(: interpret (-> (Listof Op) (U False Integer)))
(define (interpret ops)
  (interp ops (length ops) 0 0 (set) #t))

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
         [final-res : (U False Integer) (interpret ops)]
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
