(ns reasoned-schemer.three
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
(def list? seq?)

(list? '((a) (a b) c))

;; frame 2
(list? '())

;; frame 3
(list? 's)

;; frame 4
(list? (llist 'd 'a 't 'e 's))

;; frame 5
(defn listo-one
  [l]
  (conde
   [(nullo l) s#]
   [(pairo l) (fresh [d]
                (cdro l d)
                (listo-one d))]
   [s# u#]))

;; frame 7
(defn listo
  [l]
  (conde
   [(nullo l) s#]
   [(fresh [d]
      (cdro l d)
      (listo d))]))

;; frame 8

;; law of s# -> any top level s# can be removed from a conde line

;; frame 9
(run* [x]
  (listo ['a 'b x 'd]))

;; frame 10
(run* [x]
  (listo ['a 'b x 'd]))

;; will succeed for any value of x, so x remains fresh

;; frame 11

;; - listo gets the cdr of each pair then uses recursion on the cdr
;; - when listo reached the end of the list, it succeeds
;; - x is left fresh

;; frame 12
(run* [x]
  (listo (llist 'a 'b 'c x)))

;; this expression has no value; an unbounded number of possible values could
;; be associated with x

;; frame 14
(run 1 [x]
  (listo (llist 'a 'b 'c x)))

;; frame 15
(run 1 [x]
  (listo (llist 'a 'b 'c x)))

;; frame 18
(run 5 [x]
  (listo (llist 'a 'b 'c x)))

;; frame 20
(run 7 [x y]
  (conde
   [(== 'split x) (== 'pea y)]
   [(== 'red x) (== 'bean y)]
   [(== 'green x) (== 'lentil y)]))

;; frame 21
(defn lol?
  [l]
  (every? (fn [x] (list? x)) l))

(lol? '(() () ()))
(lol? (list '() '() '()))
(lol? '(foo bar baz))
(lol? "hello world")

;; frame 22
(defn lolo
  [l]
  (conde
   [(nullo l)]
   [(fresh [a]
      (caro l a)
      (listo a))]
   [(fresh [d]
      (cdro l d)
      (lolo d))]))

;; frame 23
(run* [q]
  (fresh [x y]
    (lolo (llist (llist 'a 'b) (llist x 'c) (llist 'd y)))))

;; frame 24
(run 1 [l]
  (lolo l))

;; frame 25
(run 1 [q]
  (fresh [x]
    (lolo (llist (llist 'a 'b) x))))

;; frame 26
(run 1 [x]
  (lolo (llist (llist 'a 'b) (llist 'c 'd) x)))

;; frame 27
(run 5 [x]
  (lolo (llist (llist 'a 'b) (llist 'c 'd) x)))

;; frame 29
(run 5 [x]
  (lolo x))

;; frame 32
(defn singletono
  [l]
  (fresh [d]
    (cdro l d)
    (nullo d)))

;; frame 33
(defn loso
  [l]
  (conde
   [(nullo l)]
   [(fresh [a]
      (caro l a)
      (singletono a))]
   [(fresh [d]
      (cdro l d)
      (loso d))]))

;; frame 34
(run 1 [z]
  (loso (llist '(g) z)))

;; ??

;; freme 41
(run 4 [r]
  (fresh [w x y z]
    (loso (list '(g) (llist 'e w) (llist x y) z))
    (== [w (llist x y) z] r)))

;; ??

;; frame 43
(run 3 [out]
  (fresh [w x y z]
    (== (llist '(g) (llist 'e w) (llist x y) z) out)
    (loso out)))

;; ??

;; frame 46
(defn membero-prime
  [x l]
  (conde
   [(fresh [a]
      (caro l a)
      (== a x))]
   [(fresh [d]
      (cdro l d)
      (membero-prime x d))]))

;; frame 47
(defn membero-prime-prime
  [x l]
  (conde
   [(caro l x)]
   [(fresh [d]
      (cdro l d)
      (membero-prime-prime x d))]))

;; frame 48
(run* [q]
  (membero-prime-prime 'olive (list 'extra 'olive 'oil)))

;; frame 49
(run 1 [y]
  (membero-prime-prime y (list 'hummus 'with 'pita)))

;; frame 50
(run 1 [y]
  (membero-prime-prime y (list 'with 'pita)))

;; frame 51
(run 1 [y]
  (membero-prime-prime y '(pita)))

;; frame 52
(run 1 [y]
  (membero-prime-prime y '()))

;; frame 53
(run* [y]
  (membero-prime-prime y (list 'hummus 'with 'pita)))

;; frame 55
(run* [y]
  (membero-prime-prime y (llist 'pear 'grape 'peaches)))

;; frame 56
(run* [x]
  (membero-prime-prime 'e (list 'pasta x 'rotelle)))

;; frame 59
(run 1 [x]
  (membero-prime-prime 'e (list 'pasta 'e x 'rotelle)))

;; frame 60
(run 1 [x]
  (membero-prime-prime 'e (list 'pasta x 'e 'rotelle)))

;; frame 61
(run* [x y]
  (membero-prime-prime 'e (list 'pasta x 'rotelle y)))

;; frame 63
(run* [q]
  (fresh [x y]
    (== (list 'pasta x 'rotelle y) q)
    (membero-prime-prime 'e q)))

;; frame 64
(run 1 [l]
  (membero-prime-prime 'tofu l))

;; frame 65
(run* [l]
  (membero-prime-prime 'tofu l))

;; no value, because run* never finishes building the list

;; frame 67
(run 5 [l]
  (membero-prime-prime 'tofu l))

;; frame 73
(defn proper-membero
  [x l]
  (conde
   [(caro l x) (fresh [d]
                 (cdro l d)
                 (listo d))]
   [(fresh [d]
      (cdro l d)
      (proper-membero x d))]))

;; frame 74
(run 12 [l]
  (proper-membero 'tofu l))

;; frame 75
(defn proper-member?
  [x l]
  (cond
    (null? l) false
    (== (car l) x) (-> l cdr list?)
    :else (recur x (cdr l))))
