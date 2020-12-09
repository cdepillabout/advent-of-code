
#lang racket/base
(require racket/contract)
(require racket/function)
(require racket/list)
(require racket/port)
(require racket/set)
(require racket/string)

(module+ test
  (require rackunit)
  )

(define required-names-set (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define/contract (validate-num str min max)
  (-> string? number? number? boolean?)
  (let ([num (string->number str)])
    (and num (<= min num max))
    )
  )

(module+ test
  (check-true (validate-num "100" 50 150))
  (check-false (validate-num "49" 50 150))
  (check-true (validate-num "150" 50 150))
  (check-false (validate-num "151" 50 150))
  )

(define/contract (is-digit? c)
  (char? . -> . boolean?)
  (char<=? #\0 c #\9))

(define/contract (is-all-digit? chars len)
  (-> string? number? boolean?)
  (and
   (andmap is-digit? (string->list chars))
   (equal? len (length (string->list chars)))
   )
  )

(define/contract (is-hex? c)
  (-> char? boolean?)
  (or (is-digit? c) (char<=? #\a c #\f)))

(define/contract (is-all-hex? chars len)
  (-> string? number? boolean?)
  (and
   (andmap is-hex? (string->list chars))
   (equal? len (length (string->list chars)))
   )
  )

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
(define/contract (validate-byr val)
  (-> string? boolean?)
  (validate-num val 1920 2002))

;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(define/contract (validate-iyr val)
  (-> string? boolean?)
  (validate-num val 2010 2020))

;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(define/contract (validate-eyr val)
  (-> string? boolean?)
  (validate-num val 2020 2030))

;; hgt (Height) - a number followed by either cm or in:
(define (validate-hgt val)
  ;;   If cm, the number must be at least 150 and at most 193.
  (define (validate-cm val) (validate-num val 150 193))
  ;;   If in, the number must be at least 59 and at most 76.
  (define (validate-in val) (validate-num val 59 76))
  (let ([is-cm (string-suffix? val "cm")]
        [is-in (string-suffix? val "in")]
        [trim-cm (string-trim val "cm" #:left? #f)]
        [trim-in (string-trim val "in" #:left? #f)]
        )
    (cond
      [is-cm (validate-cm trim-cm)]
      [is-in (validate-in trim-in)]
      [else #f]
      )
    )
  )

;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(define (validate-hcl val)
  (let ([is-leading-hash (string-prefix? val "#")]
        [trim-hash (string-trim val "#" #:right? #f)]
        )
    (and is-leading-hash (is-all-hex? trim-hash 6))
    )
  )

;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(define (validate-ecl val)
  (if (member val '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
      #t
      #f)
  )

;; pid (Passport ID) - a nine-digit number, including leading zeroes.
(define (validate-pid val) (is-all-digit? val 9))

(define (validate-vals names-vals)
  (define (find-name-val name names-vals)
    (second (findf (λ(nv)(equal? name (first nv))) names-vals)))
  (and
   (validate-byr (find-name-val "byr" names-vals))
   (validate-iyr (find-name-val "iyr" names-vals))
   (validate-eyr (find-name-val "eyr" names-vals))
   (validate-hgt (find-name-val "hgt" names-vals))
   (validate-hcl (find-name-val "hcl" names-vals))
   (validate-ecl (find-name-val "ecl" names-vals))
   (validate-pid (find-name-val "pid" names-vals))
   )
  )

(define (contains-required-fields? names-vals)
  (let* ([just-names (map first names-vals)])
    (and
     (subset? required-names-set (list->set just-names))
     (validate-vals names-vals)
     )
  ))

(define (is-valid pass)
  (let* ([fields (string-split pass)]
         [field-names-values (map (λ(x)(string-split x ":")) fields)]
         [contains-required-fields (contains-required-fields? field-names-values)]
         )
    contains-required-fields
    )
  )

(define (main)
  (let* (
         [in (open-input-file "day04-input")]
         ;; [in (open-input-file "day04-input-example")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str "\n\n")]
         [valids (map is-valid split-input)]
         [valids-count (count identity valids)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "valids: ~v\n" valids)
    (printf "valids-count: ~v\n" valids-count)
    )
  )

;; (module+ main
  (main)
  ;; )
