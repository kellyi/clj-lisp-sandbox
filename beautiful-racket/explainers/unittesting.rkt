#lang br

(require rackunit)
(define (plus x y) (+ x y))
(check-equal? (plus 10 14) 24)
;; (check-equal? (plus 10 10) 24)

(check-eq? 'a 'a)
(check-true (symbol? 'a))

(check-= (/ 10 2) 5 0)

(check-exn exn:fail:contract? (lambda () (/ 25 0)))
