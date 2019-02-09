(ns fun-data-stuctures.leftist-heap
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(defn create-binomial-heap-node
  "Create a node in a binomial heap"
  [rank value children]
  {:rank rank
   :value value
   :children children})

(defn link
  "Link two binomial heap nodes"
  [t1 t2]
  (cond
    (<= (:value t1) (:value t2)) (create-binomial-heap-node (inc (:rank t1))
                                                            (:value t1)
                                                            (cons t2 (:children t1)))
    :else (create-binomial-heap-node (inc (:rank t1))
                                     (:value t2)
                                     (cons t1 (:children t2)))))

(defn ins-tree
  ""
  [node [head & tail :as tree]]
  (cond
    (nil? head) node
    :else (cond
            (< (:rank node) (:rank head)) (cons node tree)
            :else (recur (link node head) tail))))

(defn mrg
  ""
  [ts1 ts2]
  (cond
    (empty? ts1) ts2
    (empty? ts2) ts1
    :else (let [[h1 & t1] ts1
                [h2 & t2] ts2]
            (cond
              (< (:rank h1) (:rank h2)) (mrg t1 ts2)
              (< (:rank h2) (:rank h1)) (mrg ts1 t2)
              :else (ins-tree (link t1 t2) (mrg t1 t2))))))

(defn remove-min-tree
  ""
  [[head & tail]]
  (cond
    (nil? head) '(nil, nil)
    (empty? tail) '(head '())
    :else (let [[h1 & t1] (remove-min-tree tail)]
            (cond
              (< (:root head) (:root h1)) '(head tail)
              :else '(h1 (cons head t1))))))

(defn insert
  ""
  [x ts]
  (ins-tree (create-binomial-heap-node 0 x '()) ts))

(defn find-min
  ""
  [ts]
  (:root (first (remove-min-tree ts))))

(defn delete-min
  ""
  [ts]
  (let [[h & t] (remove-min-tree ts)]
    (mrg (reverse (:children h)) t)))

(def one (create-binomial-heap-node 0 1 '()))
(find-min one)
