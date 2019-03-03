(ns reasoned-schemer.four
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.utils :as utils :only [pair?
                                                 pairo
                                                 null?
                                                 nullo
                                                 car
                                                 cdr
                                                 caro
                                                 cdro]]))

;; frame 1
(defn append
  [l t]
  (cond
    (null? l) t
    :else (cons (car l)
                (append (cdr l) t))))

(append '(a b c) '(d e))
(append (list 'a 'b 'c) (list 'd 'e))

;; frame 2
(append (list 'a 'b 'c) (empty list))

;; frame 3
(append (empty list) (list 'd 'e))

;; frame 4
;; (append 'a (list 'd 'e)) won't work

;; frame 5
;; (append (list 'd 'e) 'a) works in Scheme but not in Clojure

;; frame 8
(defn appendo-prime
  [l t out]
  (conde
   [(nullo t) (== t out)]
   [(fresh [res]
      (fresh [d]
        (cdro l d)
        (appendo-prime d t res))
      (fresh [a]
        (caro l a)
        (conso a res out)))]))

;; frame 13
(defn appendo-prime-prime
  [l t out]
  (conde
   [(nullo l) (== t out)]
   [(fresh [a d res]
      (conso a d l)
      (conso a res out)
      (appendo-prime-prime d t res))]))

;; frame 17
(run 6 [x]
  (fresh [y z]
    (appendo-prime-prime x y z)))

;; frame 18
(run 6 [y]
  (fresh [x z]
    (appendo-prime-prime x y z)))

;; frame 20
(run 6 [z]
  (fresh [x y]
    (appendo-prime-prime x y z)))

;; frame 21
(run 6 [x y z]
  (appendo-prime-prime x y z))

;; frame 22
(run* [x]
  (appendo-prime-prime '(cake)
                       '(tastes yummy)
                       x))

;; frame 23
(run* [x]
  (fresh [y]
    (appendo-prime-prime (list 'cake '& 'ice y)
                         '(tastes yummy)
                         x)))

;; frame 24
(run* [x]
  (fresh [y]
    (appendo-prime-prime '(cake & ice cream)
                         y
                         x)))

;; frame 25
(run 1 [x]
  (fresh [y]
    (appendo-prime-prime (llist 'cake '& 'ice y)
                         '(d t)
                         x)))

;; frame 26
(run 5 [x]
  (fresh [y]
    (appendo-prime-prime (llist 'cake '& 'ice y)
                         '(d t)
                         x)))

;; frame 27
(run 5 [y]
  (fresh [x]
    (appendo-prime-prime (llist 'cake '& 'ice y)
                         '(d t)
                         x)))

;; frame 30
(run 5 [x]
  (fresh [y]
    (appendo-prime-prime (llist 'cake '& 'ice y)
                         (llist 'd 't y)
                         x)))

;; frame 31
(run* [x]
  (fresh [z]
    (appendo-prime-prime '(cake & ice cream)
                         (llist 'd 't z)
                         x)))

;; frame 33
(run 6 [x]
  (fresh [y]
    (appendo-prime-prime x
                         y
                         '(cake & ice d t))))

;; frame 35
(run 6 [y]
  (fresh [x]
    (appendo-prime-prime x
                         y
                         '(cake & ice d t))))

;; frame 37
(run 6 [x y]
  (appendo-prime-prime x
                       y
                       '(cake & ice d t)))

;; frame 39
(run 7 [x y]
  (appendo-prime-prime x
                       y
                       '(cake & ice d t)))

;; frame 43
(defn swappendo
  [l t out]
  (conde
   [(fresh [a d res]
      (conso a d l)
      (conso a res out)
      (swappendo d t res))]
   [(nullo l) (== t out)]))

;; frame 44
(run* [x y]
  (swappendo x y '(cake & ice d t)))

;; frame 45
(defn unwrap
  [x]
  (cond
    (pair? x) (-> x car unwrap)
    :else x))

(unwrap '(((pizza))))

;; frame 46
(unwrap '((((pizza pie) with)) garlic))

;; frame 47
(defn unwrapo
  [x out]
  (conde
   [(fresh [a]
      (caro x a)
      (unwrapo a out))]
   [(== x out)]))

;; frame 48
(run* [x]
  (unwrapo '((((pizza)))) x))

;; frame 50
(run 1 [x]
  (unwrapo x 'pizza))

;; frame 51
(run 1 [x]
  (unwrapo (-> x list list) 'pizza))

;; frame 52
(run 5 [x]
  (unwrapo x 'pizza))

;; frame 53
(run 5 [x]
  (unwrapo x '((pizza))))

;; frame 54
(run 5 [x]
  (unwrapo (-> x list list) 'pizza))
