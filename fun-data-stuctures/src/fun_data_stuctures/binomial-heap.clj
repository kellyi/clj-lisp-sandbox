(ns fun-data-stuctures.binomial-heap
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(defn create-binomial-heap-node
  "Create a node in a binomial heap"
  [rank value children]
  {:rank rank
   :value value
   :children (or children '())})

(defn link
  "Create a link between two binomial heap nodes"
  [node1 node2]
  (cond
    (<= (:value node1) (:value node2)) (create-binomial-heap-node
                                        (-> node1 :rank inc)
                                        (:value node1)
                                        (cons node2 (:children node1)))
    :else (create-binomial-heap-node
           (-> node1 :rank inc)
           (:value node2)
           (cons node1 (:children node2)))))

(defn ins-tree
  "Insert a new binomial heap node into a tree"
  [node tree]
  (cond
    (empty? tree) (cons node '())
    (< (:rank node) (-> tree first :rank)) (cons node tree)
    :else (recur (link node (first tree)) (rest tree))))

(defn insert
  "Insert a new value into a binomial heap tree"
  [value tree]
  (ins-tree (create-binomial-heap-node 0 value nil) tree))

(defn merge-heaps
  "Merge two binomial heaps"
  [tree1 tree2]
  (cond
    (empty? tree1) tree2
    (empty? tree2) tree1
    (< (-> tree1 first :rank) (-> tree2 first :rank)) (cons
                                                       (first tree1)
                                                       (merge-heaps (rest tree1) tree2))
    (> (-> tree1 first :rank) (-> tree2 first :rank)) (cons
                                                       (first tree2)
                                                       (merge-heaps tree1 (rest tree2)))
    :else (ins-tree (link (first tree1) (first tree2))
                          (merge-heaps (rest tree1) (rest tree2)))))

(defn remove-min-tree
  "Remove the minimum tree from a binomial heap, returning a tuple of the minimum tree and the remaining tree"
  [[t & ts]]
  (cond
    (empty? ts) [t '()]
    :else (let [[t-prime ts-prime] (remove-min-tree ts)]
            (cond
              (<= (:value t) (:value t-prime)) [t ts]
              :else [t-prime (cons t ts-prime)]))))

(defn find-min
  "Find the minimum tree in a binomial heap"
  [ts]
  (let [[t _] (remove-min-tree ts)]
    t))

(defn delete-min
  "Remove the minimum tree from a binomial heap, returning the merged remaining trees"
  [ts]
  (let [[min-node ts-two] (remove-min-tree ts)]
    (merge-heaps (-> min-node :children reverse) ts-two)))

(def node-one (create-binomial-heap-node 0 1 nil))
(def node-five (create-binomial-heap-node 0 5 nil))
(def node-two (create-binomial-heap-node 0 2 nil))
(def node-seven (create-binomial-heap-node 0 7 nil))

(def tree-one [(link node-one node-five)
               (link node-two node-seven)])

(def tree-two [(link (create-binomial-heap-node 0 10 nil)
                     (create-binomial-heap-node 0 100 nil))
               (link (create-binomial-heap-node 0 20 nil)
                     (create-binomial-heap-node 0 15 nil))])

(def m (as-> (merge-heaps tree-one tree-two) tree
         (insert -1 tree)
         (insert -100 tree)
         (insert -50 tree)))

(-> tree-two delete-min delete-min delete-min find-min)
(-> m delete-min delete-min delete-min delete-min find-min)
