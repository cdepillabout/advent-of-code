
#lang racket

(define required-names-set (set "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define (contains-required-fields? names-vals)
  (let* ([just-names (map first names-vals)])
    (subset? required-names-set (list->set just-names))
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
  (let* ([in (open-input-file "day04-input")]
         [input-str (port->string in #:close? #t)]
         [split-input (string-split input-str "\n\n")]
         [valids (map is-valid split-input)]
         [valids-count (count identity valids)]
         ;; [couples (create-couples nums)]
         ;; [couples-sum (map sum-couple couples)]
         ;; [filtered-couples (filter is-2020 couples-sum)]
         )
    (printf "input-str: ~v\n" input-str)
    (printf "split-input: ~v\n" split-input)
    (printf "valids: ~v\n" valids)
    (printf "valids-count: ~v\n" valids-count)
    )
  )

(main)
