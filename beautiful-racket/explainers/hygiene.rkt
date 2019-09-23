#lang br

(define x 42)
(define-macro (mac)
  #'(begin
      (define x 84)
      (println x)))

(mac)
(println x)

;; (define-unhygienic-macro (mac)
;;   #'(begin
;;       (define x 84)
;;       (println x)))

(define-macro (mac-prime OUTER-X)
  #'(begin
      (define x 84)
      (println x)
      (println OUTER-X)))
(mac-prime x)

(define-macro (define-$ (ID ARG ...) BODY ...)
  (define id$-datum (format-datum '~a$ (syntax->datum #'ID)))
  (with-pattern ([ID$ (datum->syntax #'ID id$-datum)])
    #'(define (ID$ ARG ...)
        BODY ...)))

(define-$ (f x) (* x x))

(f$ 5)
