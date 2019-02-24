(ns reasoned-schemer.utils
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn pair?
  [x]
  (or (lcons? x) (and (coll? x) (seq x))))

(def car first)
(def cdr rest)
(def caro firsto)
(def cdro resto)

(defn null?
  [x]
  (or (nil? x) (empty? x)))

(def nullo emptyo)
