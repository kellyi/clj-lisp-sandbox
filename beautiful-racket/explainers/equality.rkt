#lang racket
(equal? "foobar" (string-append "foo" "bar"))
(equal? '(1 2 3 4) (append '(1 2) '(3 4)))
(equal? 42 (* 6 7))

(eq? "foobar" (string-append "foo" "bar"))
(eq? 'symbol (string->symbol "symbol"))

(eqv? "a" "a")
(eqv? 4 4)

(= 42 42)
; (= "a" "a")
