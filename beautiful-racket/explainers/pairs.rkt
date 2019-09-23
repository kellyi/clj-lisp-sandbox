#lang racket

(cons 42 43)
'(42 . 43)
'(this . that)

(car (cons 42 43))
(cdr (cons 42 43))

(define (identity my-pair)
  (equal? (cons (car my-pair)
                (cdr my-pair))
          my-pair))

(identity (cons 42 43))
