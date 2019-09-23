#lang br

(define-macro (report EXPR)
  #'(begin
      (displayln (format "input was ~a" 'EXPR))
      EXPR))

(report (* 1 2 3 4))

(define-macro-cases and
  [(and) #'#t]
  [(and COND) #'COND]
  [(and COND OTHER-CONDS ...) #'(if COND
                                    (and OTHER-CONDS ...)
                                    #f)])

(and)
(and 42)
(and 42 #f)
(and 42 #f #t)
