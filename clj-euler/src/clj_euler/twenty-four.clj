(ns clj-euler.twenty-four
  (:use [clojure.math.combinatorics :only [permutations]]))

(defn get-nth-permutation-of-list
  "Given a list l, get permutation at position n."
  [l n]
  (nth (permutations l) (dec n)))

(defn solve
  "Get the n permutation of digits 0-9."
  [n]
  (clojure.string/join (get-nth-permutation-of-list (range 0 10)
                                                    n)))

(solve 1000000)
