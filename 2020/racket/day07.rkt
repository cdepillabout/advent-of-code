#lang racket
(require data/applicative)
(require data/either)
(require data/functor)
(require data/monad)
(require graph)
(require megaparsack megaparsack/text)

;; data/functor clobbers map, so import it as racket-map.
(require (only-in racket/base (map racket-map)))

(module+ test
  (require rackunit)
  )

;; light red bags contain 1 bright white bag, 2 muted yellow bags.
;; bright white bags contain 1 shiny gold bag.
;; faded blue bags contain no other bags.

;;;;;;;;;;;;;
;; Parsing ;;
;;;;;;;;;;;;;

(define single-color/p
  (map list->string (many+/p letter/p)))

(module+ test
  (check-equal? (parse-string single-color/p "hello tomorrow") (success "hello"))
  )

(define/contract color/p
  (parser/c char? string?)
  (do
      [desc <- single-color/p]
      space/p
      [col <- single-color/p]
      (pure (string-append desc " " col))
      )
  )

(module+ test
  (check-equal? (parse-string color/p "hello tomorrow")
                (success "hello tomorrow"))
  )

(define/contract contained-bag/p
  (parser/c char? (list/c string? number?))
  (do
      [num-bags <- integer/p]
      space/p
    [col <- color/p]
    space/p
    (or/p (try/p (string/p "bags"))
          (string/p "bag"))
    (pure (list col num-bags)))
  )

(module+ test
  (check-equal? (parse-string
                 contained-bag/p
                 "1 bright white bag"
                 )
                (success '("bright white" 1))
                )
  )

(define contained-bag/c (list/c string? number?))
(define contained-bags/c (listof contained-bag/c))

(define/contract contained-bags/p
  (parser/c char? contained-bags/c)
  (do [contained <- (or/p (many+/p contained-bag/p #:sep (string/p ", "))
                         (map (const '()) (string/p "no other bags")))]
      (char/p #\.)
      (pure contained)
    )
  )

(module+ test
  (check-equal? (parse-string
                 contained-bags/p
                 "1 bright white bag, 2 muted yellow bags."
                 )
                (success '(("bright white" 1)
                           ("muted yellow" 2)
                           )
                         )
                )
  (check-equal? (parse-string
                 contained-bags/p
                 "no other bags."
                 )
                (success '())
                )
  )

(define/contract contains/p
  (parser/c char? (list/c string? contained-bags/c))
  (do
      [color <- color/p]
      (string/p " bags contain ")
      [bags <- contained-bags/p]
      (pure (list color bags))
    )
  )

(module+ test
  (check-equal? (parse-string
                 contains/p
                 "light red bags contain 1 bright white bag, 2 muted yellow bags."
                 )
                (success '("light red"
                           (("bright white" 1)
                            ("muted yellow" 2)))))
  (check-equal? (parse-string
                 contains/p
                 "bright white bags contain 1 shiny gold bag."
                 )
                (success '("bright white"
                           (("shiny gold" 1)))))
  (check-equal? (parse-string
                 contains/p
                 "faded blue bags contain no other bags."
                 )
                (success '("faded blue" ())))
  )

(define/contract all-input/p
  (parser/c char? (listof (list/c string? contained-bags/c)))
  (many+/p contains/p #:sep (string/p "\n")))

;;;;;;;;;;;;;;;;;
;; Graph Stuff ;;
;;;;;;;;;;;;;;;;;

;; (graphviz (directed-graph '((a b) (b c) (b x) (x z) (x y) (z zz) (y zz) (d c)) '(10 20 100 999 2000 3000 9999 30)) #:output (open-output-file "day07-example-graph.dot"))
;;
;; (dijkstra (directed-graph '((a b) (b c) (b x) (x z) (x y) (z zz) (y zz) (d c)) '(10 20 100 999 2000 3000 9999 30)) 'b)
;; '#hash((a . +inf.0) (b . 0) (c . 20) (d . +inf.0) (x . 100) (y . 2100) (z . 1099) (zz . 4099))
;; '#hash((a . #f) (b . #f) (c . b) (d . #f) (x . b) (y . x) (z . x) (zz . z))

(define graph-edge-ends/c (list/c string? string?))
(define graph-input-item/c (list/c graph-edge-ends/c number?))

(define/contract (build-graph-one-contained container contained)
  (-> string? contained-bag/c graph-input-item/c)
  (let ([contained-color (car contained)]
        [contained-num (cadr contained)]
        )
    (list (list container contained-color) contained-num)
    )
  )

(module+ test
  (check-equal? (build-graph-one-contained "light red" '("bright white" 1))
                '(("light red" "bright white") 1))
  )

(define/contract (build-graph-input-one-line input)
  (-> (list/c string? contained-bags/c) (listof graph-input-item/c))
  (let ([fst-color (car input)]
        [contained (cadr input)]
        )
    (racket-map (λ(one) (build-graph-one-contained fst-color one)) contained)
    )
  )

(module+ test
  (check-equal? (build-graph-input-one-line
                 '("light red" (("bright white" 1) ("muted yellow" 2))))
                '((("light red" "bright white") 1)
                  (("light red" "muted yellow") 2)))
  (check-equal? (build-graph-input-one-line
                 '("faded blue" ()))
                '())
  )

(define/contract (build-graph input-list)
  (-> (listof (list/c string? contained-bags/c)) graph?)
  (let ([input-info (append* (racket-map build-graph-input-one-line input-list))])
    ;; (printf "jfjfjf: ~v\n" i)
    (define-values (all-edges all-weights)
      (for/fold ([edges '()]
                 [weights '()])
                ([i input-info])
        (values (cons (car i) edges) (cons (cadr i) weights))
        )
      )
    (directed-graph all-edges all-weights)
    )
  )

(define/contract (how-many-can-contain orig-graph search-color)
  (-> graph? string? any/c)
  ;; Remove nodes from the graph that don't have a correct connection
  ;; to the search-color.
  (define-values (num-cons _) (bfs orig-graph search-color))
  (for ([(node num) num-cons])
    (when (infinite? num)
      (remove-vertex! orig-graph node)))

  ;; A graph with all the directions flipped.  We flip the directions
  ;; here to be able to find which nodes this node points to in the
  ;; loop below.
  (define/contract graph graph? (transpose orig-graph))

  ;; Here is a property to track for each node.  This is the total of
  ;; all the nodes linking to it.
  (define-vertex-property graph sum-total #:init 0)

  ;; Topological sorting of nodes.  This is the order we have to go
  ;; through and look at each node.
  (define just-nodes-sorted (tsort graph))

  (printf "just-nodes-sortted: ~v\n" just-nodes-sorted)

  ;; node is a string and it is the current node we are operating on
  (for ([node just-nodes-sorted])
    ;; curr-node-val is the total in this node
    (define curr-node-val (sum-total node))
    (define/contract links-here
      (listof string?)
      (get-neighbors graph node))
    (printf "currently working on node ~v, with value ~v, linking to ~v\n" node curr-node-val links-here)
    ;; link is a string for a node that connects here
    (for ([link links-here])
      ;; the weight between these two nodes
      (define/contract weight number? (edge-weight graph node link))
      ;; the current total for the link node
      (define/contract curr-link-val number? (sum-total link))
      ;; the new total for the link node
      (define/contract new-link-val number? (+ curr-link-val
                                              weight
                                              (* weight curr-node-val)))
      (printf
       "    working on ~v, weight ~v, curr val of ~v: ~v, new val: ~v\n"
       link weight curr-link-val link new-link-val)
      (sum-total-set! link new-link-val)
      )
    )
  (sum-total search-color)
  )

(define (do-dijkstra graph search-color)
  (define-values (all-connections xxx) (dijkstra graph search-color))
  (list all-connections xxx)
  )

(define (do-my-bfs graph search-color)
  (define-values (all-connections xxx) (bfs graph search-color))
  (list all-connections xxx)
  )

(define (main)
  (let* (
         ;; [in (open-input-file "day07-input")]
         [in (open-input-file "day07-input-example")]
         ;; [in (open-input-file "day07-input-example2")]
         ;; [in (open-input-file "day07-input-example3")]
         [input-str (port->string in #:close? #t)]
         [input-lines (parse-result! (parse-string all-input/p input-str))]
         [graph (build-graph input-lines)]
         ;; [color "muted yellow"]
         [color "shiny gold"]
         [graph-stuff-dijkstra (do-dijkstra graph color)]
         [graph-stuff-bfs (do-my-bfs graph color)]
         [how-many (how-many-can-contain graph color)]
         ;; [group-sum (for/sum ((g group-amount)) g)]
         ;; [seat-ids (map get-seat-id split-input)]
         ;; [max-seat-id (argmax identity seat-ids)]
         ;; [missing-seat-ids (get-missing-seat-ids seat-ids max-seat-id)]
         )
    ;; (printf "input-str: ~v\n" input-str)
    ;; (printf "input-lines: ~v\n" input-lines)
    ;; (printf "graph-stuff-dijkstra: ~v\n" graph-stuff-dijkstra)
    ;; (printf "graph-stuff-bfs: ~v\n" graph-stuff-bfs)
    (printf "how-many: ~v\n" how-many)
    (call-with-output-file "day07-graph.dot" #:exists 'replace
      (λ (out-file) (graphviz graph #:output out-file)))
    ;; (printf "split-input: ~v\n" split-input)
    ;; (printf "groups: ~v\n" groups)
    ;; (printf "group-amount: ~v\n" group-amount)
    ;; (printf "group-sum: ~v\n" group-sum)
    '()
    )
  )

;; (module+ main
(main)
;; )
