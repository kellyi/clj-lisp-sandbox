#lang racket

'(a b c)
'{a b c}
'[a b c]

(let ([x 42]
      [y 58])
  (* x y))

(list * 42 59)
(list 1 2 3)
(list)
(list 'a 'b 'c)

(quote (* 42 58))
'(* 42 58)

(list 1 2 3 (list (list 'a 'b 'c) 4 5 6))
'(1 2 3 ((a b c) 4 5 6))

(list + expt)

(list 1 2 (list "a" "b") +)
(list 1 2 '("a" "b") +)

(define x 42)
'(41 x 43)
`(41 ,x 43)

(define xs (list 42 43 44))
`(41 ,@xs 45)
`(41 ,xs 45)

(list? '())
(list? '(1 2 3))
(list? 123)

(empty? '())
(empty? '(1 2 3))
(empty? 123)

(length '())
(length '(1 2 3))

(flatten '(1 (2 3 (4 5 6 (7 8)))))

(apply + '(1 2 3 4 5 6 7 8 9))

(map abs (list -1 2 -3 4))
(map + '(1 2 3 4) '(10 20 30 40))

(for-each displayln (list -1 2 -3 4))

(filter odd? '(1 2 3 4 5 6 7 8 9))

(cdr '(1 2 3 4 5))
(rest '(1 2 3 4 5))

(define (square x) (expt 2 x))

(define (squarer xs)
  (if (empty? xs)
      empty
      (cons (square (first xs))
            (squarer (rest xs)))))

(squarer '(1 2 3 4 5))

(map square '(1 2 3 4 5))
