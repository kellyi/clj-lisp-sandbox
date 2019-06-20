(ns clj-euler.five)

(def one-to-ten (reverse (range 2 11)))
(def one-to-twenty (reverse (range 2 21)))

(defn solve
  "Find the smallest number evenly divisble by all in range r."
  ([] (solve 1 one-to-twenty))
  ([n r]
   (if (some (fn [x] (->> x (mod n) zero? not)) r)
     (recur (inc n) r)
     n)))

(solve)
