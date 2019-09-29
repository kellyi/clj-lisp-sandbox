#lang br
(require br/indent
         racket/contract
         racket/gui/base)

(define indent-width 2)
(define (left-bracket? c)
  (member c (list #\{ #\[)))
(define (right-bracket? c)
  (member c (list #\} #\])))

(define (indent-jsonic tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  (define current-line (line tbox posn))
  (cond
    [(not prev-line) 0]
    [else
     (define prev-indent (line-indent tbox prev-line))
     (cond
       [(left-bracket?
         (line-first-visible-char tbox prev-line))
        (+ prev-indent indent-width)]
       [(right-bracket?
         (line-first-visible-char tbox current-line))
        (- prev-indent indent-width)]
       [else prev-indent])]))

(provide
 (contract-out
  [indent-jsonic
   (((is-a?/c text%))
    (exact-nonnegative-integer?) . ->* .
    exact-nonnegative-integer?)]))

(module+ test
  (require rackunit)
  (define test-str #<<HERE
#lang jsonic
{
"value": 42,
"string":
[
{
"array": @$(range 5)$@,
"object": @$(hash 'k1 "valstring")$@
}
]
// "bar"
}
HERE
    )
  (check-equal?
   (string-indents
    (apply-indenter indent-jsonic test-str))
    '(0 0 2 2 2 4 6 6 4 2 2 0)))
