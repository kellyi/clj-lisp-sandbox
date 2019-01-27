(ns fun-data-stuctures.two
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

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

(spec/fdef create-bs-tree-node
  :args (spec/cat :value ::value
                  :left ::bs-tree-node?
                  :right ::bs-tree-node?)
  :ret ::bs-tree-node?)

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

;; ...

;; 2.3 Rewrite insert using execptions to avoid copying

;; ...

;; 2.4 Combine defer-bs-tree-member? and exception-insert? into a more efficient
;; insert function

;; ...

;; 2.5 (A) Write a function `complete` where `(complete x d)` creates a complete tree of
;; depth d with x stored in every node

(defn create-complete-tree
  "Create a complete tree of depth d with value x stored at each node"
  [x d]
  {:pre [(spec/valid? int? x)
         (spec/valid? nat-int? d)]
   :post [(spec/valid? ::bs-tree-node? %)]}
  (cond
   (zero? d) (create-bs-tree-node x nil nil)
   :else (create-bs-tree-node x
                              (create-complete-tree x (dec d))
                              (create-complete-tree x (dec d)))))

(create-complete-tree 2 10)

;; 2.5 (B)

;; ...

;; 2.6 Implement maps using a binary search tree

(spec/def ::key int?)
(spec/def ::bs-tree-map-node?
  (spec/nilable
   (spec/keys :req-un [::key
                       ::value
                       ::right (or nil? ::bs-tree-map-node?)
                       ::left (or nil? ::bs-tree-map-node?)])))

(spec/fdef create-bs-tree-map-node
  :args (spec/cat :key ::key
                  :value ::value
                  :left ::bs-tree-map-node?
                  :right ::bs-tree-map-node?)
  :ret ::bs-tree-map-node?)

(defn create-bs-tree-map-node
  "Create a new map entry node for a binary search tree"
  [key value left right]
  {:pre [(spec/valid? ::key key)
         (spec/valid? ::value value)
         (spec/valid? ::bs-tree-map-node? left)
         (spec/valid? ::bs-tree-map-node? right)]
   :post [(spec/valid? ::bs-tree-map-node? %)]}
  {:key key
   :value value
   :left left
   :right right})

(defn get-bs-tree-value-for-key
  "Given a key and a tree get the key's value or return nil"
  [key node]
  {:pre [(spec/valid? ::key key)
         (spec/valid? ::bs-tree-map-node? node)]}
  (cond
    (nil? node) nil
    (< key (:key node)) (recur key (:left node))
    (> key (:key node)) (recur key (:right node))
    :else (:value node)))

(defn add-or-update-bs-tree-map-entry
  "Given a key, a value, and a map tree, add or update the map's key-value entry"
  [key value node]
  {:pre [(spec/valid? ::key key)
         (spec/valid? ::value value)
         (spec/valid? ::bs-tree-map-node? node)]
   :post [(spec/valid? ::bs-tree-map-node? %)]}
  (cond
    (nil? node) (create-bs-tree-map-node key value nil nil)
    (< key (:key node)) (create-bs-tree-map-node (:key node)
                                                 (:value node)
                                                 (add-or-update-bs-tree-map-entry key
                                                                                  value
                                                                                  (:left node))
                                                 (:right node))
    (> key (:key node)) (create-bs-tree-map-node (:key node)
                                                 (:value node)
                                                 (:left node)
                                                 (add-or-update-bs-tree-map-entry key
                                                                                  value
                                                                                  (:right node)))
    :else (create-bs-tree-map-node key value (:left node) (:right node))))

(def squares (as-> (create-bs-tree-map-node 5 25 nil nil) tree
               (add-or-update-bs-tree-map-entry 7 49 tree)
               (add-or-update-bs-tree-map-entry 3 9 tree)
               (add-or-update-bs-tree-map-entry 1 1 tree)
               (add-or-update-bs-tree-map-entry 2 4 tree)
               (add-or-update-bs-tree-map-entry 6 36 tree)
               (add-or-update-bs-tree-map-entry 4 16 tree)))

(get-bs-tree-value-for-key 7 (add-or-update-bs-tree-map-entry 7 100 squares))

(defn count-bs-map-tree-entries
  "Count the number of entries in a map tree"
  ([tree] (count-bs-map-tree-entries tree 0))
  ([tree acc]
   {:pre [(spec/valid? ::bs-tree-map-node? tree)]
    :post [(spec/valid? nat-int? %)]}
   (cond
     (nil? tree) acc
     :else (+ 1
              acc
              (count-bs-map-tree-entries (:left tree))
              (count-bs-map-tree-entries (:right tree))))))

(count-bs-map-tree-entries squares)
