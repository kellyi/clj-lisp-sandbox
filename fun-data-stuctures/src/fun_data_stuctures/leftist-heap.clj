(ns fun-data-stuctures.leftist-heap
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(spec/def ::value int?)
(spec/def ::rank nat-int?)
(spec/def ::leftist-heap-node?
  (spec/nilable
   (spec/keys :req-un [::rank
                       ::value
                       ::right (or nil? ::leftist-heap-node?)
                       ::left (or nil? ::leftist-heap-node?)])))

(spec/fdef create-leftist-heap-node
  :args (spec/cat :rank ::rank
                  :value ::value
                  :left ::leftist-heap-node?
                  :right ::leftist-heap-node?)
  :ret ::leftist-heap-node?)

(defn create-leftist-heap-node
  "Create a new leftist-heap node"
  [rank value left right]
  {:pre [(spec/valid? ::rank rank)
         (spec/valid? ::value value)
         (spec/valid? ::leftist-heap-node? left)
         (spec/valid? ::leftist-heap-node? right)]
   :post [(spec/valid? ::leftist-heap-node? %)]}
  {:rank rank
   :value value
   :left left
   :right right})

(def single-node (create-leftist-heap-node 1 100 nil nil))

(defn leftist-heap-is-empty?
  "Check whether the heap with node at the apex is empty"
  [node]
  {:pre [(spec/valid? ::leftist-heap-node? node)]}
  (nil? node))

(defn find-min
  "Find the minimum value in a leftist heap"
  [{value :value :as node}]
  {:pre [(spec/valid? ::leftist-heap-node? node)
         (spec/valid? int? value)]}
  value)

(defn make-t
  "Helper function which creates new nodes while merging heaps"
  [value left right]
  {:pre [(spec/valid? ::leftist-heap-node? left)
         (spec/valid? ::leftist-heap-node? right)]
   :post [(spec/valid? ::leftist-heap-node? %)]}
  (cond
    (>= (:rank left 0) (:rank right 0)) (create-leftist-heap-node (inc (:rank right 0))
                                                                  value
                                                                  left
                                                                  right)
    :else (create-leftist-heap-node (inc (:rank left 0))
                                    value
                                    right
                                    left)))

(defn merge-heaps
  "Merge two leftist heaps"
  [first-heap second-heap]
  {:pre [(spec/valid? ::leftist-heap-node? first-heap)
         (spec/valid? ::leftist-heap-node? second-heap)]
   :post [(spec/valid? ::leftist-heap-node? %)]}
  (cond
    (nil? first-heap) second-heap
    (nil? second-heap) first-heap
    (<= (:value first-heap) (:value second-heap)) (make-t (:value first-heap)
                                                          (:left first-heap)
                                                          (merge-heaps (:right first-heap)
                                                                       second-heap))
    :else (make-t (:value second-heap)
                  (:left second-heap)
                  (merge-heaps (:right second-heap)
                               first-heap))))

(defn insert-value
  "Insert a new value in a leftist heap"
  [value heap]
  {:pre [(spec/valid? ::value value)
         (spec/valid? ::leftist-heap-node? heap)]
   :post [(spec/valid? ::leftist-heap-node? %)]}
  (merge-heaps (create-leftist-heap-node 0 value nil nil) heap))

(defn delete-min
  "Return a new leftist heap with the minimum value of the prior heap removed"
  [{left :left right :right}]
  (merge-heaps left right))

(def l-heap
  (as-> (create-leftist-heap-node 0 6 nil nil) l-tree
    (insert-value 8 l-tree)
    (insert-value 7 l-tree)
    (insert-value 14 l-tree)
    (insert-value -20 l-tree)))

(def r-heap
  (as-> (create-leftist-heap-node 0 100 nil nil) r-tree
    (insert-value 20 r-tree)
    (insert-value 50 r-tree)
    (insert-value -50 r-tree)
    (insert-value 40 r-tree)
    (insert-value -100 r-tree)
    (delete-min r-tree)))

(delete-min (merge-heaps l-heap r-heap))
