(ns fun-data-structures.pairing-heap
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(def empty-heap-node nil)
(def is-empty-heap-node? nil?)

(defn create-heap-node
  ([value] {:value value :heap '()})
  ([value heap] {:value value :heap heap}))

(defn merge-heaps
  [{value1 :value heap1 :heap :as h1}
   {value2 :value heap2 :heap :as h2}]
  (cond
    (is-empty-heap-node? h1) h2
    (is-empty-heap-node? h2) h1
    (< value1 value2) (create-heap-node value1 (conj heap1 h2))
    :else (create-heap-node value2 (conj heap2 h1))))

(defn insert-value
  [value heap]
  (merge-heaps (create-heap-node value) heap))

(defn merge-pairs
  [[fst snd & tail]]
  (cond
    (nil? fst) empty-heap-node
    (nil? snd) fst
    :else (merge-heaps (merge-heaps fst snd)
                       (merge-pairs tail))))

(defn find-min
  [heap]
  (cond
    (is-empty-heap-node? heap) nil
    :else (:value heap)))

(defn delete-min
  [heap]
  (cond
    (is-empty-heap-node? heap) nil
    :else (-> heap :heap merge-pairs)))

(def test-data (->> (create-heap-node 100)
                    (insert-value 1000)
                    (insert-value 50)
                    (insert-value 1)
                    (insert-value -20)
                    (insert-value 20)))

(->> test-data
     (insert-value -50)
     (insert-value -70)
     (insert-value 5000)
     delete-min
     delete-min
     delete-min)
