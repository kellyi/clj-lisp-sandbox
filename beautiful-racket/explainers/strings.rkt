#lang at-exp br

"one string"
"two \"strings\""

"one literal string\n"
"two literal \"strings\""

(display "one literal string\n")
(displayln "one literal string\n")
(print "one literal string\n")
(println "one literal string\n")

(define str #<<here-string-delimiter
A wonderful
multiline string
that goes on
seemingly
forever.
here-string-delimiter
  )

(display str)

(define mult-str @string-append{
                                A wonderful
                                multiline string
                                that goes on
                                seemingly
                                forever.
                                })

(display mult-str)

(define base 2)
(define pow 8)

(define pow-str @string-append{
                               A wonderful
                               multiline string
                               that goes on
                               @(number->string (expt base pow)) years.
                               })

(display pow-str)

'symbol
'|two-word symbol|

(define (f #:base x #:pow y)
  (expt x y))

(f #:base 2 #:pow 8)
(keyword-apply f '(#:base #:pow) '(2 8) empty)
(keyword-apply f (list
                  (string->keyword "base")
                  (string->keyword "pow"))
               '(2 8)
               empty)

; comment
#|
a
multline
comment
|#
