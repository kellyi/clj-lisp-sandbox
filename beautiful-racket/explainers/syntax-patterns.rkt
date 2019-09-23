#lang br

(define-macro (m foo) #'"match")
(m foo)
; (m bar)

(define-macro-cases m2
  [(m2) "first"]
  [(m2 foo) "second"]
  [(m2 foo ARG) #'ARG]
  [else "no match"])

(m2)
(m2 foo)
(m2 foo 'hello)
(m2 hello)

(define-macro (m3 MID ... LAST)
  (with-pattern ([(ONE TWO THREE) (syntax LAST)]
                 [(ARG ...) #'(MID ...)])
    #'(list ARG ... THREE TWO ONE)))

(m3 25 42 ("foo" "bar" "baz"))

(define-macro-cases sym
  [(sym 'foo) "match"]
  [else "no-match"])

(sym 'foo)
(sym 'bar)

(define-macro (add-three ARG)
  #'(+ ARG ARG ARG))

(add-three 42)

(define-macro (odds FIRST _ THIRD _ FIFTH)
  #'(list FIRST THIRD FIFTH))

(odds 1 2 3 4 5)

(define-macro (ellip ARG ...)
  #'(list ARG ...))

(ellip 1 2 3)
(ellip "a" "b")
(ellip)

(define-macro (d . ARGS) #'ARGS)
(d + 1 2 3)

(define-macro (d2 . ARGS) #'(list . ARGS))
(d2 1 2 3)
