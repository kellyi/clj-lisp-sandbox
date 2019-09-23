#lang br
(provide (all-defined-out))
(define my-var 42)
(define another-var 96)
(define third-var 256)

(module bad br
  (define bad-plus -)
  (provide (rename-out [bad-plus +])))

(require (submod "." bad))
(+ 42 42)

(require racket/bool
         (for-syntax racket/bool))
(define-macro (nander)
  #'(println (and (nand #f #t) 'phase-0)))
(nander)

(define-macro (nander-prime)
  (println (and (nand #f #t) 'phase-1))
  #'(println (and (nand #f #t) 'phase-0)))

(nander-prime)
