(ns clj-euler.thirty-four)

(defn digits
  "Get digits in a number."
  ([n] (digits n '()))
  ([n digits-list]
   (if (< n 10)
     (cons n digits-list)
     (recur
      (quot n 10)
      (cons (rem n 10)
            digits-list)))))

(defn find-factorial
  "Find the factorial of n."
  [n]
  (cond
    (zero? n) 1
    (= n 1) n
    :else (* n (factorial (dec n)))))

(def factorial
  "Memoized function to return the factorial of a number."
  (memoize find-factorial))

(defn sum-digits-factorials
  "Sum the factorials of the digits of n."
  [n]
  (->> n
       digits
       (map factorial)
       (apply +)))

(defn solve
  "Find the sum of numbers less than 10,000,000 equal to the sums of the
  factorials of their digits."
  []
  (apply
   +
   (filter #(= % (sum-digits-factorials %))
           (range 3 10000000))))

(solve)
