#lang br

(module mod br
  (provide x)
  (define x (make-parameter 42)))

(require 'mod)
(x)
(x 43)
(x)

(define current-list (make-parameter '(0 1 2 3 4)))
(displayln (car (current-list)))
(parameterize ([current-list '(a b c d e)])
  (displayln (car (current-list))))
(displayln (car (current-list)))

(module fun br
  (provide f)
  (define x (make-parameter 42))
  (define (f) (x)))

(require 'fun)

(parameterize ([x 101])
  (println (x))
  (println (f)))

(require racket/stxparam)

(module param-prime br
  (require racket/stxparam)
  (provide y)
  (define-syntax-parameter y (lambda (stx) #'42)))

(require 'param-prime)
