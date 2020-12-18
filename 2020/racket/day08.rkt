
#lang typed/racket

;; (struct pt ([x : Real] [y : Real]))

;; (define-type Tree (U leaf node))
;; (struct leaf ([val : Number]))
;; (struct node ([left : Tree] [right : Tree]))

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

(: 2list-inner? (-> (Listof String) Boolean))
(define (2list-inner? lst)
  (if (eq? (length lst) 2) #t #f))

;; (: 2list? (-> (Listof String) Boolean : (List String String)))
;; (define 2list?
;;   (cast 2list-inner? (-> (Listof String) Boolean : (List String String))))

;; (: 2list? (-> (Listof String) Boolean : #:+ ((List String String) @ 0) #:- Bot))
;; (: 2list? (-> (Listof String) Boolean : #:+ ((List String String) @ 0) #:- Bot))
;; (: 2list? (-> (Listof String) Boolean : (List String String)))
;; (define (2list? lst)
;;   (if (pair? lst)
;;       (let ([xxx : (Listof String) (cdr lst)])
;;         (if (pair? xxx)
;;             (let ([yyy : (Listof String) (cdr xxx)])
;;               (if (null? yyy) #t #f))
;;             #f))
;;       #f))

;; This works!
;; (: 2list? (-> (Listof String) Boolean : #:+ Null))
;; (: 2list? (-> (Listof String) Boolean : #:+ (Null @ 0)))
;; (: 2list? (-> (Listof String) Boolean : Null))
;; (define (2list? lst)
;;   (if (null? lst) #t #f))

;; (: 2list? (All [A] (-> (Listof A) Boolean : (List A))))
;; (define (2list? lst)
;;   (if (and (pair? lst)
;;            ;; (string? (car lst))
;;            (null? (cdr lst)))
;;       #t
;;       #f))

(: 2list? (All [A] (-> (Listof A) Boolean : (List A A))))
(define (2list? lst)
  (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst))))

(define-predicate 2list-foo? (List String String))

;; (: main (-> Null))
;; (define (main)
;;   (let* (
;;          ;; [in (open-input-file "day08-input")]
;;          [in (open-input-file "day08-input-example")]
;;          [input-str (port->string in #:close? #t)]
;;          [lines (string-split input-str "\n")]
;;          [instructions
;;           (map
;;            (λ([x : String]) : (List String String) (string-split x " "))
;;            lines
;;            )]
;;          [ops (map to-ops instructions)]
;;          )
;;     ;; (printf "input-str: ~v\n" input-str)
;;     ;; (printf "lines: ~v\n" lines)
;;     (printf "instructions: ~v\n" instructions)
;;     (printf "ops: ~v\n" ops)
;;     '()
;;     )
;;   )

;; (main)
