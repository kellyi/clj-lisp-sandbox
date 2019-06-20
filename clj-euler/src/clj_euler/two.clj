(ns clj-euler.two)

(defn solve
  "Sum the even Fibonacci numbers below 4,000,000"
  ([] (solve 1 2 0))
  ([prior current accumulator]
   (if (> current 4000000)
     accumulator
     (recur current
            (+ prior current)
            (+ accumulator
               (if (even? current)
                 current
                 0))))))

(solve)
