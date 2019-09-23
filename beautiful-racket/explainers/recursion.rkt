#lang racket

(define (factorial n)
  (define product 1)
  (for ([i (in-naturals 1)]
        #:final (= i n))
    (set! product (* product i)))
  product)

(factorial 5)

(define (factorial-prime n)
  (if (= n 1)
      1
      (* n (factorial-prime (- n 1)))))

(factorial-prime 5)

(define (my-map proc xs)
  (if (null? xs)
      null
      (cons (proc (first xs))
            (my-map proc (rest xs)))))

(my-map (lambda (x) (* x x)) '(1 2 3 4 5))

(define (my-filter pred xs)
  (if (null? xs)
      null
      (if (pred (first xs))
          (cons (first xs) (my-filter pred (rest xs)))
          (my-filter pred (rest xs)))))

(my-filter (lambda (x) (zero? (remainder x 2)))
           '(1 2 3 4 5))

(define (tail-factorial n [acc 1])
  (if (= n 1)
      acc
      (tail-factorial (- n 1) (* n acc))))

(tail-factorial 5)

(define (tail-sum n [acc 0])
  (if (zero? n)
      acc
      (tail-sum (- n 1) (+ n acc))))
