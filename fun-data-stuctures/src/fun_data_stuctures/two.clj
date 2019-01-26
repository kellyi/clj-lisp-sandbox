(ns fun-data-stuctures.two
  (:require [clojure.spec.alpha :as spec]))

(defn ++
  "Concatenate two lists into a new list"
  [[x & xs] ys]
  (cond
    (nil? x) ys
    :else (cons x (++ xs ys))))

(++ '(1 2 3) '(4 5 6))

(defn fun-update
  "Create a new list which replaces the value at index i with y"
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
  "Return a list of all suffixes of a list in decreasing length order"
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
  "Create a new binary search tree node"
  [value left right]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::bs-tree-node? left)
         (spec/valid? ::bs-tree-node? right)]
   :post [(spec/valid? ::bs-tree-node? %)]}
  {:value value
   :left left
   :right right})

(defn bs-tree-member?
  "Check whether a value is a member of the set stored in a binary search tree"
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
(bs-tree-member? 20 single-node)

(defn bs-tree-insert
  "Create a new node to store the given value in a binary search tree"
  [value node]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::bs-tree-node? node)]
   :post [(spec/valid? ::bs-tree-node? %)]}
  (cond
    (nil? node) (create-bs-tree-node value nil nil)
    (< value (:value node)) (create-bs-tree-node (:value node)
                                                 (bs-tree-insert value (:left node))
                                                 (:right node))
    (> value (:value node)) (create-bs-tree-node (:value node)
                                                 (:left node)
                                                 (bs-tree-insert value (:right node)))
    :else node))

(def multi-node (as-> single-node tree
                  (bs-tree-insert -2 tree)
                  (bs-tree-insert 10 tree)
                  (bs-tree-insert 5 tree)
                  (bs-tree-insert 1000 tree)
                  (bs-tree-insert 50 tree)
                  (bs-tree-insert -50 tree)
                  (bs-tree-insert 10 tree)))

;; 2.2 adjust tree-member? function to defer equality check until the end
(defn has-right-node?
  "Check whether a binary search tree node has a right node"
  [node]
  {:pre [(spec/valid? ::bs-tree-node? node)]}
  (cond
    (nil? (:right node)) false
    :else true))

(defn has-left-node?
  "Check whether a binary search tree node has a left node"
  [node]
  {:pre [(spec/valid? ::bs-tree-node? node)]}
  (cond
    (nil? (:left node)) false
    :else true))

(defn is-leaf-node?
  "Check whether a binary search tree node is a leaf node"
  [node]
  {:pre [(spec/valid? ::bs-tree-node? node)]}
  (and (-> node has-left-node? not)
       (-> node has-right-node? not)))

;; (defn defer-bs-tree-member?
;;   "check whether a value is a member of the set stored in a binary search tree"
;;   [value node]
;;   {:pre [(spec/valid? ::value value)
;;          (spec/valid? ::bs-tree-node? node)]}
;;   (cond
;;     (< value (:value node)) (recur value (:left node))
;;     :else (recur value (:right node))))

(is-leaf-node? single-node)
(is-leaf-node? multi-node)

;; 2.3 Rewrite insert using execptions to avoid copying

;; ...

;; 2.4 Combine defer-bs-tree-member? and exception-insert? into a more efficient
;; insert function

;; ...

;; 2.5 Write a function `complete` where `(complete x d)` creates a complete tree of
;; depth d with x stored in every node

(defn create-complete-tree
  "Create a complete tree of depth d with value x stored at each node"
  [x d]
  {:pre [(spec/valid? int? x)
         (spec/valid? int? x)]
   :post [(spec/valid? ::bs-tree-node? %)]}
  (cond
   (zero? d) (create-bs-tree-node x nil nil)
   :else (create-bs-tree-node x
                              (create-complete-tree x (dec d))
                              (create-complete-tree x (dec d)))))

(create-complete-tree 10 5)
