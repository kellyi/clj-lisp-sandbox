#lang racket
(complex? 3)
(real-part 3)
(imag-part 3)
(imag-part 3+0i)
(= 3 3+0i)

(* +i +i)

(+ 1/3 1/5)
(exact? 3)
(exact? 3+4i)
(exact? (sin 1))
(exact? (* 3.0 3.0))
(apply < (range 100))

#x10
#o20
#b1000

(number->string 3+0i)
(string->number "3+0i")
