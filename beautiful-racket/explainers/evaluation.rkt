#lang br

(require br/verbose-app)
(* (+ 1 2) (* 3 4 (/ 5 6)))

(define-macro (omacro INNER-EXPR) #'(displayln "outer macro"))
(define-macro (imacro) #'(displayln "inner macro"))

(omacro (imacro))
