(ns reasoned-schemer.nine
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [reasoned-schemer.eight :as eight]
        [reasoned-schemer.utils :as utils :only [pair?
                                                 pairo
                                                 null?
                                                 nullo
                                                 car
                                                 cdr
                                                 caro
                                                 cdro]]))
(defn alwayso
  []
  (conde
   [s#]
   [(alwayso)]))

;; frame 1
(conda
 (u# s#)
 (s# u#))

;; frame 5
(run* [x]
  (conda
   [(== 'olive x) s#]
   [s# (== 'oil x)]))

;; frame 7
(run* [x]
  (conda
   [(== 'extra x) u#]
   [(== 'olive x) s#]
   [s# (== 'oil x)]))

;; frame 8
(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== 'split x) (== x y)]
     [s# s#])))

;; frame 9
(run* [q]
  (fresh [x y]
    (== 'split x)
    (== 'pea y)
    (conda
     [(== x y) (== 'split x)]
     [s# s#])))

;; frame 11
(defn not-pastao
  [x]
  (conda
   [(== 'pasta x) u#]
   [s# s#]))

(run* [x]
  (conda
   [(not-pastao x) u#]
   [(== 'spaghetti x) s#]))

;; frame 12
(run* [x]
  (== 'spaghetti x)
  (conda
   [(not-pastao x) u#]
   [(== 'spaghetti x) s#]))


;; frame 13
(run* [q]
  (conda
   [(alwayso) s#]
   [s# u#]))

;; frame 14
(run* [q]
  (condu
   [(alwayso) s#]
   [s# u#]))

;; frame 15
(run* [q]
  (condu
   [s# (alwayso)]
   [s# u#]))

;; frame 17
(run* [q]
  (conda
   [(alwayso) s#]
   [s# u#]))

;; frame 18
(run 1 [q]
  (condu
   [(alwayso) s#]
   [s# u#])
  u#)

;; frame 20
(defn teacupo
  [t]
  (conde
   [(== 'tea t)]
   [(== 'cup t)]))

;; frame 21
(defn onceo-prime
  [g]
  (condu
   [g s#]
   [s# u#]))

(run* [x]
  (-> x teacupo onceo-prime))

;; frame 22
(run* [r]
  (conde
   [(teacupo r) s#]
   [(== :#f r) s#]))

;; frame 23
(run* [r]
  (conda
   [(teacupo r) s#]
   [(s# (== :#f r))]))

;; frame 24
(run* [r]
  (== :#f r)
  (conda
   [(teacupo r) s#]
   [(== :#f r) s#]
   [s# u#]))

;; frame 25
(run* [r]
  (== :#f r)
  (condu
   [(teacupo r) s#]
   [(== :#f r) s#]
   [s# u#]))

;; frame 26
(defn bumpo
  [n x]
  (conde
   [(== n x)]
   [(fresh [m]
      (eight/minuso n (list 1) m)
      (bumpo m x))]))

(run* [x]
  (bumpo '(1 1 1) x))
;; ?

;; frame 27
(defn gen&test+o
  [i j k]
  (onceo-prime
   (fresh [x y z]
     (eight/pluso x y z)
     (== i x)
     (== j y)
     (== k z))))

;; (run* [q]
;;   (gen&test+o '(0 0 1) '(1 1) '(1 1 1)))
;; ?

;; frame 43
(defn enumerate+o
  [r n]
  (fresh [i j k]
    (bumpo n i)
    (bumpo n j)
    (eight/pluso i j k)
    (gen&test+o i j k)
    (== (list i j k) r)))

(run* [s]
  (enumerate+o s '(1 1)))

;; frame 56
(run 1 [s]
  (enumerate+o s '(1 1 1)))

;; frame 59
(defn enumerate+o-prime
  [r n]
  (fresh [i j k]
    (bumpo n i)
    (bumpo n j)
    (eight/pluso i j k)
    (onceo-prime
     (fresh [x y z]
       (eight/pluso x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== (list i j k) r)))

;; frame 60
(defn enumerateo
  [op r n]
  (fresh [i j k]
    (bumpo n i)
    (bumpo n j)
    (op i j k)
    (onceo-prime
     (fresh [x y z]
       (op x y z)
       (== i x)
       (== j y)
       (== k z)))
    (== (list i j k) r)))
