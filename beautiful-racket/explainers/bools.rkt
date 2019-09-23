#lang racket

(if #f 'true 'false)
(if "#f" 'true 'false)
(if "false" 'true 'false)
(if 0 'true 'false)
(if null 'true 'false)
(if "" 'true 'false)
(if (+ 0 0) 'true 'false)

(if (not #f) 'true 'false)
(if (not #t) 'true 'false)
(if (not 0) 'true 'false)
(if (not "") 'true 'false)

(and #f 'short-circuit)
(and #t 'short-circuit)

(andmap odd? '(1 2 3))
(ormap even? '(1 2 3))

(if (= 42 (* 6 8))
    'true-result
    'false-result)

(if (= 42 (* 6 7))
    "foo"
    42
    ;; 'true-result
    ;; 'false-result
    )

(if (= 42 (* 6 7))
    (let ()
      "foo"
      42
      'true-result)
    'false-result)

(when (= 42 (* 6 7))
  "foo"
  42
  'true-result)

(cond
  [(= 1 1) 'hello]
  [else 'else-result])

(define (c x)
  (case x
    [(foo bar) 'got-symbol]
    [("zim" "zam") 'got-string]
    [(42 84) 'got-number]
    [else 'got-nothing]))

(c 'foo)
(c "zim")
(c 42)
(c #t)

(struct thing (x y))

(define (m in)
  (match in
    ["foo" 'got-foo]
    [(? number?) 'got-number]
    [(list a b c) (list b)]
    [(thing i j) (+ i j)]
    [else 'no-match]))

(m "foo")
(m 42)
(m (list 1 2 3))
(m (thing 25 52))
(m "bar")
