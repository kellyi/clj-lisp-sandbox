#lang racket

(define vec (vector 'sym "str" 42))
vec

(vector-ref vec 0)
(vector-ref vec 2)

(vector-set! vec 2 'bar)

(define ht (hash
            'foo "foo"
            'bar '(1 2 3)
            'baz expt))

(hash-ref ht 'foo)
((hash-ref ht 'baz) 2 3)

(define alists
  (list
   (cons 'k1 'v1)
   (cons 'k2 'v2)))

alists

(define keys (map car alists))
(define vals (map cdr alists))
keys
vals

(assoc 'k1 alists)

(struct style (color size weight) #:transparent #:mutable)

(define s (style "red" 42 'bold))
s
(style? s)
(style-color s)
(set-style-color! s "blue")
(style-color s)
