(ns clj-euler.twenty)

(defn sum-digits
  "Sum the digits of a number n."
  ([n] (sum-digits n 0))
  ([n acc]
   (if (< n 10)
     (+ n acc)
     (recur (quot n 10) (+ acc (rem n 10))))))

(defn factorial
  "Find the factorial of n."
  [n]
  (if (zero? n)
    1
    (*' n (factorial (dec n)))))

(defn solve
  "Sum the digits of the factorial of n."
  [n]
  (-> n factorial sum-digits))

(solve 100)
