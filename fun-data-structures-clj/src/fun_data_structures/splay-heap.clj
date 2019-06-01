(ns fun-data-structures.splay-heap
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(defn make-splay-heap-node
  [right value left]
  {:value value
   :right right
   :left left})

(def empty-node nil)

(defn is-empty-node?
  [node]
  (nil? node))

(defn partition-tree
  [pivot tree]
  (cond
    (is-empty-node? tree) [empty-node empty-node]
    (and (<= (:value tree) pivot)
         (-> tree :right is-empty-node?)) [tree empty-node]
    (and (<= (:value tree) pivot)
         (<= (-> tree :right :value) pivot)) (let [[small big] (partition-tree pivot (-> tree :right :right))]
                                               [(make-spay-heap-node tree
                                                                     (-> tree :right :value)
                                                                     small)
                                                big])
    (<= (:value tree) pivot) (let [[small big] (partition-tree pivot (-> tree :right :left))]
                               [(make-spay-heap-node (-> tree :left)
                                                     (-> tree :value)
                                                     small)
                                (make-splay-heap-node big
                                                      (-> tree :right :value)
                                                      (-> tree :right :right))])
    (-> tree :left is-empty-node?) [empty-node tree]
    (<= (-> tree :left :value) pivot) (let [[small big] (partition-tree pivot (-> tree :left :right))]
                                         [(make-splay-heap-node (-> tree :left :left)
                                                                (-> tree :left :value)
                                                                small)
                                          (make-splay-heap-node big
                                                                (-> tree :value)
                                                                (-> tree :right))])
    :else (let [[small big] (partition-tree pivot (-> tree :left :left))]
            [small
             (make-splay-heap-node big
                                   (-> tree :left :value)
                                   (make-splay-heap-node (-> tree :left :right)
                                                         (-> tree :value)
                                                         (-> tree :right)))])))

(defn insert-value
  [x t]
  (let [[a b] (partition-tree x t)]
    (make-splay-heap-node a
                          x
                          b)))

(defn merge-trees
  [t1 t2]
  (cond
    (is-empty-node? t1) t2
    :else (let [[ta tb] (partition-tree (-> t1 :value) t2)]
            (make-splay-heap-node (merge-trees ta (-> t1 :left))
                                  (-> t1 :value)
                                  (merge-trees tb (-> t1 :right))))))

(defn find-min
  [t]
  (cond
    (is-empty-node? t) nil
    (-> t :left is-empty-node?) (-> t :value)
    :else (recur (-> t :left))))

(defn delete-min
  [t]
  (cond
    (is-empty-node? t) nil
    (-> t :left is-empty-node?) (-> t :right)
    (-> t :left :left is-empty-node?) (make-splay-heap-node (-> t :left :right)
                                                            (-> t :value)
                                                            (-> t :right))
    :else (make-splay-heap-node (-> t :left :left delete-min)
                                (-> t :left :value)
                                (make-splay-heap-node (-> t :left :right)
                                                      (-> t :value)
                                                      (-> t :right)))))

(def base-tree (make-splay-heap-node nil 1 nil))
(-> (insert-value 50 (insert-value 5 base-tree)) delete-min)

