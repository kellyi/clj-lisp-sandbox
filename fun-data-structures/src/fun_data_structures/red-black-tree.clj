(ns fun-data-stuctures.red-black-tree
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(def red :red)
(def black :black)

(defn create-node
  "Create a node in a red/black tree"
  [color left value right]
  {:color color
   :left left
   :value value
   :right right})

(defn member
  "Check whether a value x is a member of a red/black tree"
  [x tree]
  (cond
    (nil? tree) false
    (empty? tree) false
    (< x (:value tree)) (recur x (:left tree))
    (> x (:value tree)) (recur x (:right tree))
    :else true))

(defn balance
  "Balance nodes in a red/black tree"
  [tree]
  (cond
    (and
     (= red (-> tree :left :color))
     (= red (-> tree :left :left :color))) (create-node red
                                                        (create-node black
                                                                     (-> tree :left :left :left)
                                                                     (-> tree :left :left :value)
                                                                     (-> tree :left :left :right))
                                                        (-> tree :left :value)
                                                        (create-node black
                                                                     (-> tree :left :right)
                                                                     (-> tree :value)
                                                                     (-> tree :right)))
    (and
     (= red (-> tree :left :color))
     (= red (-> tree :left :right :color))) (create-node red
                                                         (create-node black
                                                                      (-> tree :left :left)
                                                                      (-> tree :left :value)
                                                                      (-> tree :left :right :left))
                                                         (-> tree :left :right :value)
                                                         (create-node black
                                                                      (-> tree :left :right :right)
                                                                      (-> tree :value)
                                                                      (-> tree :right)))
    (and
     (= red (-> tree :right :color))
     (= red (-> tree :right :left :color))) (create-node red
                                                         (create-node black
                                                                      (-> tree :left)
                                                                      (-> tree :value)
                                                                      (-> tree :right :left :left))
                                                         (-> tree :right :left :value)
                                                         (create-node black
                                                                      (-> tree :right :left :right)
                                                                      (-> tree :right :value)
                                                                      (-> tree :right :right)))
    (and
     (= red (-> tree :right :color))
     (= red (-> tree :right :right :color))) (create-node red
                                                          (create-node black
                                                                       (-> tree :left)
                                                                       (-> tree :value)
                                                                       (-> tree :right :left))
                                                          (-> tree :right :value)
                                                          (create-node black
                                                                       (-> tree :right :right :left)
                                                                       (-> tree :right :right :value)
                                                                       (-> tree :right :right :right)))
    :else tree))

(defn insert
  "Insert a new value in a red/black tree"
  [x s]
  (letfn [(ins
              [s-prime]
              (cond
                (or (nil? s-prime) (empty? s-prime)) (create-node red nil x nil)
                (< x (:value s-prime)) (balance (create-node (:color s-prime)
                                                             (-> s-prime :left ins)
                                                             (:value s-prime)
                                                             (:right s-prime)))
                (> x (:value s-prime)) (balance (create-node (:color s-prime)
                                                             (:left s-prime)
                                                             (:value s-prime)
                                                             (-> s-prime :right ins)))
                :else s-prime))]
    (let [s-prime-prime (ins s)]
      (create-node black
                  (:left s-prime-prime)
                  (:value s-prime-prime)
                  (:right s-prime-prime)))))

(def red-black-tree (->> (create-node red nil 25 nil)
                         (insert 5)
                         (insert 10)
                         (insert 20)
                         (insert 0)))

(member 45 red-black-tree)
(member 25 red-black-tree)
