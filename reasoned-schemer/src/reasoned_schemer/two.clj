(ns reasoned-schemer.two
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.utils :as utils :only [pair?
                                                 null?
                                                 nullo
                                                 car
                                                 cdr
                                                 caro
                                                 cdro]]))

;; frame 1
(car '(grape raisin pair))

;; frame 2
(car '(a c o r n))

;; frame 3
(run* [q]
  (caro '(a c o r n) q))

;; frame 4
(run* [q]
  (caro '(a c o r n) 'a))

;; frame 5
(run* [r]
  (fresh [x y]
    (caro [r y] x)
    (== 'pear x)))

;; frame 7
(cons (car '(grape raising pear))
      (car '((a) (b) (c))))

;; frame 8
(run* [r]
  (fresh [x y]
    (caro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (lcons x y) r)))

;; frame 10
(cdr '(grape raisin pear))

;; frame 11
(-> '(a c o r n) cdr cdr car)

;; frame 12
(run* [r]
  (fresh [v]
    (cdro '(a c o r n) v)
    (fresh [w]
      (cdro v w)
      (caro w r))))

;; frame 14
(cons (cdr '(grape raisin pear))
      (car '((a) (b) (c))))

;; frame 15
(run* [r]
  (fresh [x y]
    (cdro '(grape raisin pear) x)
    (caro '((a) (b) (c)) y)
    (== (lcons x y) r)))

;; frame 16
(run* [q]
  (cdro '(a c o r n) '(c o r n)))

;; frame 17
(run* [x]
  (cdro ['c 'o 'r 'n] [x 'r 'n]))

;; frame 18
(run* [l]
  (fresh [x]
    (cdro l '(c o r n))
    (caro l x)
    (== 'a x)))

;; frame 19
(run* [l]
  (conso '(a b c) '(d e) l))

;; frame 20
(run* [x]
  (conso x '(a b c) '(d a b c)))

;; frame 21
(run* [r]
  (fresh [x y z]
    (== ['e 'a 'd x] r)
    (conso y ['a z 'c] r)))

;; frame 22
(run* [x]
  (conso x ['a x 'c] ['d 'a x 'c]))

;; frame 23
(run* [l]
  (fresh [x]
    (== ['d 'a x 'c] l)
    (conso x ['a x 'c] l)))

;; frame 24
(run* [l]
  (fresh [x]
    (conso x ['a x 'c] l)
    (== ['d 'a x 'c] l)))

;; frame 25
(defn my-conso
  [a d p]
  (caro p a)
  (cdro p d))

;; frame 27
(run* [l]
  (fresh [d t x y w]
    (conso w ['n 'u 's] t)
    (cdro l t)
    (caro l x)
    (== 'b x)
    (cdro l d)
    (caro d y)
    (== 'o y)))

;; frame 28
(null? '(grape raisin pear))

;; frame 29
(null? '())

;; frame 30
(run* [q]
  (nullo '(grape raisin pear)))

;; frame 31
(run* [q]
  (nullo '()))

;; frame 32
(run* [x]
  (nullo x))

;; frame 41
(car '(pear))

;; frame 42
(cdr '(pear))

;; frame 44
(lcons '(split) 'pea)

;; frame 45
(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))

;; frame 46
(defn pairo
  [p]
  (fresh [a d]
    (conso a d p)))

;; frame 47
(run* [q]
  (pairo (lcons q q)))

;; frame 48
(run* [q]
  (pairo '()))

;; frame 49
(run* [q]
  (pairo 'pair))

;; frame 50
(run* [x]
  (pairo x))

;; frame 51
(run* [r]
  (pairo (lcons r '())))

;; frame 58
(defn singleton?
  [l]
  (cond
    (pair? l) (null? (cdr l))
    :else false))

(singleton? '((a) (a b) c))

;; frame 60
(singleton? '())

;; frame 61
(singleton? (cons 'pea '()))

;; frame 62
(singleton? '(sauerkraut))

;; frame 68
(defn singletono
  [l]
  (fresh [d]
    (cdro l d)
    (nullo d)))
