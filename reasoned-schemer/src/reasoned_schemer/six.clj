(ns reasoned-schemer.six
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
(defn alwayso
  []
  (conde
   [s#]
   [(alwayso)]))

;; frame 2
(run 1 [q]
  (conde
   [s#]
   (alwayso)))

;; frame 4
(run* [q]
  (alwayso))

;; frame 5
(run* [q]
  (conde
   [s#]
   [(alwayso)]))

;; frame 6
(run 5 [q]
  (alwayso))

;; frame 7
(run 5 [q]
  (== 'onion q)
  (alwayso))

;; frame 8
;; (run 1 [q]
;;   (alwayso)
;;   u#)
;; recurs endlessly

;; frame 9
(run 1 [q]
  (== 'garlic q)
  s#
  (== 'onion q))

;; frame 10
;; (run 1 [q]
;;   (== 'garlic q)
;;   (alwayso)
;;   (== 'onion q))
;; recurs endlessly

;; frame 11
(run 1 [q]
  (conde
   [(== 'garlic q) (alwayso)]
   [(== 'onion q)])
  (== 'onion q))

;; frame 12
;; (run 2 [q]
;;   (conde
;;    [(== 'garlic q) (alwayso)]
;;    [(== 'onion q)])
;;   (== 'onion q))

;; frame 13
(run 5 [q]
  (conde
   [(== 'garlic q) (alwayso)]
   [(== 'onion q) (alwayso)])
  (== 'onion q))

;; frame 14
(defn anyo
  [g]
  (conde
   [g s#]
   [(anyo g)]))

(defn nevero
  []
  (anyo u#))

;; frame 16
;; (run 1 [q]
;;   (nevero))
;; recurs endlessly

;; frame 17
;; (run 1 [q]
;;   u#
;;   (nevero))

;; frame 18
(run 1 [q]
  (conde
   [s#]
   [(nevero)]))

;; frame 19
(run 1 [q]
  (conde
   [(nevero)]
   [s#]))

;; frame 20
;; (run 2 [q]
;;   (conde
;;    [s#]
;;    [(nevero)]))

;; frame 21
;; (run 1 [q]
;;   (conde
;;    [s#]
;;    [(nevero)])
;;   u#)

;; frame 22
(run 5 [q]
  (conde
   [(nevero)]
   [(alwayso)]
   [(nevero)]))

;; frame 23
(run 6 [q]
  (conde
   [(== 'spicy q) (nevero)]
   [(== 'hot q) (nevero)]
   [(== 'apple q) (alwayso)]
   [(== 'cider q) (alwayso)]))

;; frame 24
(defn very-recursiveo
  []
  (conde
   [(nevero)]
   [(very-recursiveo)]
   [(alwayso)]
   [(very-recursiveo)]
   [(nevero)]))

;; frame 25
(run 1000000 [q]
  (very-recursiveo))
