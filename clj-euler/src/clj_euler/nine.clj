(ns clj-euler.nine)

(defn square [x] (* x x))

(defn factor-pairs
  "Find factor pairs of n."
  ([n] (factor-pairs n 1 (vector)))
  ([n counter pairs]
   (cond
     (= counter n) pairs
     (< (quot n counter) counter) pairs
     (->> counter (rem n) zero?) (recur n
                                        (inc counter)
                                        (conj pairs
                                              [counter
                                               (quot n counter)]))
     :else (recur n (inc counter) pairs))))

(defn generate-triplets
  "Given r, generate Pythagorean triplets using Dickson's method."
  [r]
  (let [triplet-from-pair (fn
                            [[a b]]
                            [(+ r a) (+ r b) (+ r a b)])]
    (->
     r
     square
     (quot 2)
     factor-pairs
     (#(map triplet-from-pair %)))))

(defn solve
  "Find pythagorean triplet whose components sum to n."
  ([n] (solve n 2 (generate-triplets 2)))
  ([n counter [h & tail]]
   (cond
     (empty? h) (recur n (inc counter) (generate-triplets (inc counter)))
     (= n (apply + h)) (apply * h)
     :else (recur n counter tail))))

(solve 1000)
