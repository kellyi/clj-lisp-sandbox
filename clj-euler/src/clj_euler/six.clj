(ns clj-euler.six)

(defn solve
  "Find difference between sum of squares of the first n numbers and the square of sum."
  [n]
  (let [square (fn [x] (* x x))
        sum (fn [l] (apply + l))
        r (range 1 (+ 1 n))]
    (- (->> r sum square)
       (->> r (map square) sum))))

(solve 100)
