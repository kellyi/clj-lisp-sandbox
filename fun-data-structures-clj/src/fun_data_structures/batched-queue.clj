(ns fun-data-stuctures.batched-queue
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]))

(def empty-queue `(~(list) ~(list)))

(defn is-empty-queue
  [l]
  (-> l first nil?))

(is-empty-queue '(nil ()))

(defn check-f
  [q]
  (cond
    (-> q first empty?) `(~(-> q second reverse) ~(list))
    :else q))

(defn snoc
  [q x]
  (check-f (first q) (cons x (rest q))))

(defn head
  [q]
  (cond
    (-> q first empty?) nil
    :else (-> q first first)))

(defn tail
  [q]
  (cond
    (-> q first empty?) nil
    :else (check-f (-> q first rest) (second q))))
