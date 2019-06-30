(ns clj-euler.fifteen)

(defn factorial
  "Calculate the factorial of n."
  [n]
  (if (zero? n)
    1
    (*' n (factorial (dec n)))))

(defn n-choose-k
  "Binomial coefficient."
  [n k]
  (/ (factorial n)
     (*' (factorial k) (factorial (- n k)))))

(def solve
  "Count routes through a 20x20 grid."
  n-choose-k)

(solve 40 20)
