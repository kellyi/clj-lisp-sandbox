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

(spec/def ::value int?)
(spec/def ::bs-tree-node?
  (spec/nilable
   (spec/keys :req-un [::value
                       ::right (or nil? ::bs-tree-node?)
                       ::left (or nil? ::bs-tree-node?)])))

(defn create-bs-tree-node
  [value left right]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::bs-tree-node? left)
         (spec/valid? ::bs-tree-node? right)]
   :post [(spec/valid? ::bs-tree-node? %)]}
  {:value value
   :left left
   :right right})

(defn bs-tree-member?
  [value node]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::bs-tree-node? node)]}
  (cond
    (nil? node) false
    (< value (:value node)) (recur value (:left node))
    (> value (:value node)) (recur value (:right node))
    :else true))

(def single-node (create-bs-tree-node 20 nil nil))
(bs-tree-member? 0 single-node)
(bs-tree-member? 1 single-node)
(bs-tree-member? 2 single-node)

(defn bs-tree-insert
  [value node]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::bs-tree-node? node)]}
  (cond
    (nil? node) (create-bs-tree-node value nil nil)
    (< value (:value node)) (create-bs-tree-node (:value node)
                                                 (bs-tree-insert value (:left node))
                                                 (:right node))
    (> value (:value node)) (create-bs-tree-node (:value node)
                                                 (:left node)
                                                 (bs-tree-insert value (:right node)))
    :else node))

(as-> single-node tree
  (bs-tree-insert -2 tree)
  (bs-tree-insert 10 tree)
  (bs-tree-insert 5 tree)
  (bs-tree-insert 1000 tree)
  (bs-tree-insert 50 tree)
  (bs-tree-insert -50 tree)
  (bs-tree-insert 10 tree))
