(ns reasoned-schemer.one
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; frame 7
(run* [q]
  u#)

;; frame 10
(run* [q]
  (== 'pea 'pod))

;; frame 11
(run* [q]
  (== q 'pea))

;; frame 14
(run* [q]
  (== 'pea q))

;; frame 15
(run* [q]
  s#)

;; frame 16
(run* [q]
  s#)

;; frame 19
(run* [q]
  (== 'pea 'pea))

;; frame 20
(run* [q]
  (== q q))

;; frame 21
(run* [q]
  (fresh [x]
    (== 'pea q)))

;; frame 22
(run* [q]
  (fresh [x]
    (== 'pea x)))

;; frame 26
(run* [q]
  (fresh [x]
    (== `(~x) q)))

;; frame 32
(run* [q]
  (== [[['pea]] 'pod] [[['pea]] 'pod]))

;; frame 33
(run* [q]
  (== [[['pea]] 'pod] [[['pea]] q]))

;; frame 34
(run* [q]
  (fresh [x]
    (== [[[q]] 'pod] [[[x]] 'pod])))

;; frame 35
(run* [q]
  (fresh [x]
    (== [[[q]] x] [[[x]] 'pod])))

;; frame 37
(run* [q]
  (fresh [x]
    (== [x x] q)))

;;  frame 38
(run* [q]
  (fresh [x]
    (fresh [y]
      (== [q y] [[x y] x]))))

;; frame 40
(run* [q]
  (fresh [x]
    (== 'pea q)))

;; frame 41
(run* [q]
  (fresh [x]
    (fresh [y]
      (== [x y] q))))

;; frame 42
(run* [s]
  (fresh [t]
    (fresh [u]
      (== [t u] s))))

;; frame 43
(run* [q]
  (fresh [x]
    (fresh [y]
      (== [x y x] q))))

;; frame 44
;; doesn't succeed
(== '(pea) 'pea)

;; frame 45
;; will never succeed
;; "x cannot be equal to a list in which x occurs"
(== ['x] 'x)

;; frame 50
(run* [q]
  (and s# s#))

;; frame 51
(run* [q]
  (and*
   [s# (== 'corn q)]))

;; frame 52
(run* [q]
  (and*
   [u# (== 'corn q)]))

;; frame 53
(run* [q]
  (and*
   [(== 'corn q)
    (== 'meal q)]))

;; frame 54
(run* [q]
  (and*
   [(== 'corn q)
    (== 'corn q)]))

;; frame 55
(run* [q]
  (or*
   [u# u#]))

;; frame 56
(run* [q]
  (or*
   [(== 'olive q)
    u#]))

;; frame 57
(run* [q]
  (or*
   [(== 'oil q)
    u#]))

;; frame 58
(run* [q]
  (or*
   [(== 'olive q)
    (== 'oil q)]))

;; frame 59
(run* [q]
  (fresh [x]
    (fresh [y]
      (or*
       [(== [x y] q)
        (== [y x] q)]))))

;; frame 61
(run* [x]
  (or*
   [(== 'olive x)
    (== 'oil x)]))

(run* [x]
  (or*
   [(== 'oil x)
    (== 'olive x)]))

;; frame 62
(run* [x]
  (or*
   [(and*
     [(== 'olive x)
      u#])
    (== 'oil x)]))

;; frame 63
(run* [x]
  (or*
   [(and*
     [(== 'olive x)
      s#])
    (== 'oil x)]))

;; frame 64
(run* [x]
  (or*
   [(== 'oil x)
    (and*
     [(== 'olive x)
      s#])]))

;; frame 65
(run* [x]
  (or*
   [(and*
     [(== 'extra x)
      u#])
    (or*
     [(== 'olive x)
      (or*
       [s#
        (== 'oil x)])])]))

;; frame 67
(run* [r]
  (fresh [x]
    (fresh [y]
      (and*
       [(== 'split x)
        (and*
         [(== 'pea y)
          (== [x y] r)])]))))

;; frame 68
(run* [r]
  (fresh [x]
    (fresh [y]
      (and*
       [(and*
         [(== 'split x)
          (== 'pea y)])
        (== [x y] r)]))))

;; frame 70
(run* [r]
  (fresh [x y]
    (and*
     [(and*
       [(== 'split x)
        (== 'pea y)])
      (== [x y] r)])))

;; frame 72
(run* [r x y]
  (and*
   [(and*
     [(== 'split x)
      (== 'pea y)])
    (== [x y] r)]))

;; frame 73
(run* [x y]
  (and*
   [(== 'split x)
    (== 'pea y)]))

;; frame 74
(run* [x y]
  (or*
   [(and*
     [(== 'split x)
      (== 'pea y)])
    (and*
     [(== 'red x)
      (== 'bean y)])]))

;; frame 75
(run* [r]
  (fresh [x y]
    (and*
     [(or*
       [(and*
         [(== 'split x)
          (== 'pea y)])
        (and*
         [(== 'red x)
          (== 'bean y)])])
      (== [x y 'soup] r)])))

;; frame 78
(run* [r]
  (fresh [x y]
    (or*
     [(and*
       [(== 'split x)
        (== 'pea y)])
      (and*
       [(== 'red x)
        (== 'bean y)])])
    (== [x y 'soup] r)))

;; frame 80
(run* [x y z]
  (or*
   [(and*
     [(== 'split x)
      (== 'pea y)])
    (and*
     [(== 'red x)
      (== 'bean y)])])
  (== 'soup z))

;; frame 81
(run* [x y]
  (== 'split x)
  (== 'pea y))

;; frame 82
(defn teacupo
  [t]
  (or*
   [(== 'tea t)
    (== 'cup t)]))

;; frame 83
(run* [x]
  (teacupo x))

;; frame 84
(run* [x y]
  (or*
   [(and*
     [(teacupo x)
      (== true y)])
    (and*
     [(== false x)
      (== true y)])]))

;; frame 85
(run* [x y]
  (teacupo x)
  (teacupo y))

;; frame 87
(run* [x y]
  (or*
   [(and*
     [(teacupo x)
      (teacupo x)])
    (and*
     [(== false x)
      (teacupo y)])]))

;; frame 88
(run* [x y]
  (conde
   [(== 'split x) (== 'pea y)]
   [(== 'red x) (== 'bean y)]))

;; frame 89
(run* [x]
  (conde
   [(== 'olive x) u#]
   [(== 'oil x)]))

;; frame 90
;; Clojure core.logic's conde is actually the book's condi.
;; Core.logic offers no conde as is presented in the book.
;; This means the order of results may not match what is
;; shown in the book when you use conde.
;; https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer
(run* [x y]
  (conde
   [(fresh [z]
      (== 'lentil z))]
   [(== x y)]))

;; frame 91
(run* [x y]
  (conde
   [(== 'split x) (== 'pea y)]
   [(== 'red x) (== 'bean y)]
   [(== 'green x) (== 'lentil y)]))
