(ns clj-euler.eight)

(def input
  (->>
   "src/clj_euler/eight.input.txt"
   slurp
   (re-seq #"[0-9]")
   (map clojure.edn/read-string)))

(defn solve
  "Find the largest product from n consecutive digits in input."
  ([n] (solve n 0 input))
  ([n largest digits]
   (if (> n (count digits))
     largest
     (recur n
            (max largest (apply * (take n digits)))
            (drop 1 digits)))))

(solve 13)
