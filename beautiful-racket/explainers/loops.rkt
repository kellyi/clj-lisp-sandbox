#lang racket

(for ([i (list 1 2 3)])
  (println i))

(for ([i (list 1 2 3)]
      [j (list 1 2)])
  (println (list i j)))

(define xs (for ([i (list 1 2 3)])
             (* i i)))

(void? xs)

(define xs-prime (for/list ([i (list 1 2 3)])
                   (* i i)))
xs-prime

(define v (for/vector ([i (list 1 2 3)])
            (* i i)))

v

(define h (for/hash ([i (list 1 2 3)])
            (values i (* i i))))
h

(for/list ([i 3]) i)
(for/list ([x '(4 5 6)]) x)
(for/list ([c "bar"]) c)
(for/list ([s (regexp-match* #rx"." "bar")]) s)
(for/list ([i (in-range 3)]) i)
(for/list ([x (in-list '(4 5 6))]) x)
(for/list ([c (in-string "bar")]) c)

(for/list ([i (in-range 3)]
           #:when (odd? i))
  i)

(for/list ([i (in-range 3)]
           #:unless (odd? i))
  i)

(for/list ([i (in-naturals)]
           #:break (= i 3))
  i)

(for/list ([i (in-naturals)]
           #:final (= i 3))
  i)

(for/list ([i (in-range 3)]
           [j (in-range 3)])
  (list i j))

(for*/list ([i (in-range 3)]
            [j (in-range 3)])
  (list i j))

(map (λ (i) (* i i)) (filter odd? (range 100)))
(for/list ([i (in-range 100)]
           #:when (odd? i))
  (* i i))
