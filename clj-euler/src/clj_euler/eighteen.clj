(ns clj-euler.eighteen)

(def input
  (as-> "src/clj_euler/eighteen.input.txt" data
    (slurp data)
    (clojure.string/split data #"\n")
    (map (comp
          (fn [row] (map #(Integer/parseInt %) row))
          #(clojure.string/split % #" "))
         data)
    (reverse data)))

(defn fold-first-row-into-second
  "Fold the first row into the second."
  [f s]
  (map-indexed (fn [i x]
                 (+ x (max (nth f i)
                           (nth f (inc i)))))
               s))

(defn find-maximum-path-sum
  "Find the maximum path sum through a triangle, structured as a list."
  [l]
  (if (-> l second nil?)
    (-> l first first)
    (recur (cons (fold-first-row-into-second (first l)
                                             (second l))
                 (-> l rest rest)))))

(defn solve
  []
  (find-maximum-path-sum input))

(solve)
