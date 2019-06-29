(ns clj-euler.thirteen)

(defn solve
  "Find the first ten digits of the sum of some 50 digit numbers."
  []
  (as-> "src/clj_euler/thirteen.input.txt"
      data
    (slurp data)
    (clojure.string/split data #"\n")
    (map clojure.edn/read-string data)
    (apply +' data)
    (str data)
    (take 10 data)
    (clojure.string/join data)))

(solve)
