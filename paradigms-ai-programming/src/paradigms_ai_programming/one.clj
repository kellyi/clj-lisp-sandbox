(ns paradigms-ai-programming.core
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as string]))

;; Exercise 1.1

(defn last-name
  "Given a name, find the last name excluding title suffixes"
  [name]
  (let [suffixes #{"jr", "md", "esquire", "iii", "iv"}
        name-elements (string/split name #" ")
        last-element (last name-elements)]
    (cond
      (contains? suffixes (string/lower-case last-element)) (recur (string/join " "
                                                                                (butlast name-elements)))
      :else last-element)))

(last-name "Harry Connick JR")
(last-name "harry connick")

;; Exercise 1.2

(defn power
  "Raise an integer to a power"
  ([base exp] (power base exp 1))
  ([base exp acc]
   (cond
     (zero? exp) acc
     :else (recur base (- exp 1) (* acc base)))))

(power 3 0)
(power 3 2)
(power 2 4)

;; Exercise 1.3

(defn count-atoms
  "Count the number of elements in a quoted expression"
  ([l] (count-atoms l 0))
  ([[h & tail] acc]
   (cond
     (nil? h) acc
     :else (recur tail (inc acc)))))

(count-atoms '(1 2 3))
(count-atoms '(1 '(2) 3))
(count-atoms '(1 '()))

;; Exercise 1.4

(defn count-anywhere
  "Count the number of times an expression occurs in another expression"
  ([exp other-exp] (count-anywhere exp other-exp 0))
  ([exp [h & tail] acc]
   (cond
     (nil? h) acc
     (list? h) (+ (count-anywhere exp h 0) (count-anywhere exp tail acc))
     (= exp h) (recur exp tail (inc acc))
     :else (recur exp tail acc))))

(count-anywhere 1 '(2 '(2 1) 1 '(2 '(2 '(1)))))

;; Exercise 1.5

(defn dot-product
  "Calculate the dot product of two lists"
  ([x y] (dot-product x y []))
  ([[x & xs] [y & ys] z]
   (cond
     (nil? x) (reduce + z)
     :else (recur xs ys (conj z (* x y))))))

(dot-product '(10 20) '(3 4))
