(ns fun-data-stuctures.two
  (:require [clojure.spec.alpha :as spec]))

(defn ++
  [[x & xs] ys]
  (cond
    (nil? x) ys
    :else (cons x (++ xs ys))))

(++ '(1 2 3) '(4 5 6))

(defn fun-update
  [[x & xs] i y]
  (cond
    (nil? x) nil
    (zero? i) (cons y xs)
    :else (cons x (fun-update xs (- i 1) y))))

(fun-update '(1 2 3 4 5 6 7 8 9 10) 4 100)

;; Exercise 2.1
;;
;; Write a function `suffixes` ... that takes a list `xs` and returns a list
;; of all the suffixes of `xs` in decreasing order of length.

(defn suffixes
  ([xs] (suffixes xs []))
  ([xs accumulator]
   (cond
     (empty? xs) (conj accumulator xs)
     :else (suffixes (rest xs) (conj accumulator xs)))))

(suffixes '(1 2 3 4))

(spec/def ::bs-tree-node
  (spec/keys :req-un [::value int?
                      ::right (or nil? ::bs-tree-node)
                      ::left (or nil? ::bs-tree-node)]))

(defn create-bs-tree-node
  [value left right]
  {:post [(spec/valid? ::bs-tree-node %)]}
  {:value value
   :left left
   :right right})

(create-bs-tree-node 1 (create-bs-tree-node 2 nil nil) nil)
