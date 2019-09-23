#lang racket

(define plus +)
(define another-plus +)
(equal? plus another-plus)

(plus 30 12)
(another-plus 30 12)

(define (plus-prime x y) (+ x y))
(plus-prime 10 20)

((lambda (x y) (+ x y)) 10 20)

(define lambda-plus (lambda (x y) (+ x y)))
(lambda-plus 10 20)

(filter even? (range 5))

(filter (λ (x) (zero? (modulo x 2))) (range 5))

(define (sub x y) (- x y))
(define (kw-sub #:foo foo-val y) (sub foo-val y))
(kw-sub 12 #:foo 30)

(define (add-rest x . others)
  (apply + x others))
(add-rest 1 2 3 4 5)
(/ 1 2 3 4 5)

(define (f x . others)
  (println others))

(f 42)

(define (void-add x y) (when #f (+ x y)))
(void-add 30 12)
(void? (void-add 30 12))

(define (two-vals x y)
  (values x y))

(define-values (sum prod) (two-vals 11 13))
(list sum prod)
