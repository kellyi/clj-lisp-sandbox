(ns reasoned-schemer.five
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
(defn mem
  [x l]
  (cond
    (null? l) false
    (= (car l) x) l
    :else (recur x (cdr l))))

;; frame 2
(mem 'fig '(roll okra beet beet roll pea))

;; frame 3
(mem 'roll
     (mem 'fig '(roll okra fig beet roll pea)))

;; frame 4
(defn memo
  [x l out]
  (conde
   [(caro l x) (== l out)]
   [(fresh [d]
      (cdro l d)
      (memo x d out))]))

;; frame 5
(run* [q]
  (memo 'fig '(pea) '(pea)))

;; frame 6
(run* [out]
  (memo 'fig '(fig) out))

;; frame 7
(run* [out]
  (memo 'fig '(fig pea) out))

;; frame 8
(run* [r]
  (memo r
        '(roll okra fig beet fig pea)
        '(fig beet fig pea)))

;; frame 9
(run* [x]
  (memo 'fig '(fig pea) (list 'pea x)))

;; frame 10
(run* [x]
  (memo 'fig '(fig pea) (list x 'pea)))

;; frame 11
(run* [out]
  (memo 'fig '(beet fig pea) out))

;; frame 13
(run 1 [out]
  (memo 'fig '(fig fig pea) out))

;; frame 14
(run* [out]
  (memo 'fig '(fig fig pea) out))

;; frame 18
(run* [out]
  (fresh [x]
    (memo 'fig (list 'a x 'c 'fig 'e) out)))

;; frame 19
(run 5 [x y]
  (memo 'fig (llist 'fig 'd 'fig 'e y) x))

;; frame 23
(defn rember
  [x l]
  (cond
    (null? l) '()
    (= (car l) x) (cdr l)
    :else (cons (car l)
                (rember x (cdr l)))))

;; frame 24
(rember 'pea '(a b pea d pea e))

;; frame 25
(defn rembero-prime
  [x l out]
  (conde
   [(nullo l) (== '() out)]
   [(conso x out l)]
   [(fresh (a d res)
      (conso a d l)
      (conso a res out)
      (rembero x d res))]))

;; frame 26
(run* [out]
  (rembero-prime 'pea '(pea) out))

;; frame 27
(run* [out]
  (rembero-prime 'pea (list 'pea 'pea) out))

;; frame 28
(run* [out]
  (fresh [y z]
    (rembero y (list 'a 'b y 'd z 'e) out)))

;; verifying that rembero is working as implemented in core.logic?
(run* [q]
  (fresh [a b x y]
    (== q [a b])
    (rembero a (list :apple :banana :carrot) x)
    (rembero b x y)))

;; frame 48
(run* [y z]
  (rembero-prime y (list y 'd z 'e) (list y 'd 'e)))

;; frame 56
(run* [y z w out]
  (rembero-prime y (llist z w) out))

;; frame 61
(run* [y z w out]
  (rembero y (llist z w) out))
