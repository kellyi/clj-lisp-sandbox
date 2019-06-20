(ns clj-euler.one)

(defn solve
  "Solve PE problem 1."
  []
  (apply + (filter (fn [x]
                     (or
                      (zero? (mod x 3))
                      (zero? (mod x 5))))
                   (range 1 1000))))

(solve)
