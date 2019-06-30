(ns clj-euler.twenty-three)

(defn find-all-divisors
  "Find all divisors of n."
  [n]
  (reduce (fn [acc x]
            (if (->> x (rem n) zero?)
              (conj acc x (quot n x))
              acc))
          #{1}
          (range 2 (Math/sqrt (inc n)))))

(defn sum-divisors
  "Sum all proper divisors of n."
  [n]
  (apply + (find-all-divisors n)))

(defn is-abundant-number?
  "Is n an abundant number?"
  [n]
  (if (> (sum-divisors n) n)
    n
    nil))

(defn find-abundant-numbers-below-n
  "Find abundant numbers below n."
  [n]
  (filter is-abundant-number? (range 1 n)))

(def abundant-numbers-for-problem
  "All abundant numbers below 28123."
  (find-abundant-numbers-below-n 28123))

(defn can-be-written-as-sum-of-two-abundant-numbers
  "Returns n if n can be written as the sum of two abundant numbers."
  ([n abundant-numbers] (can-be-written-as-sum-of-two-abundant-numbers n
                                                                       abundant-numbers
                                                                       (set abundant-numbers)))
  ([n [test-value & test-values] abundant-numbers-set]
   (if (or (nil? test-value) (< n test-value))
     nil
     (let [difference (- n test-value)]
       (if (contains? abundant-numbers-set difference)
         n
         (recur n test-values abundant-numbers-set))))))

(defn solve
  "Find the count of numbers below 21823 that can't be written as the sum of two abundant numbers."
  []
  (apply +
   (remove #(can-be-written-as-sum-of-two-abundant-numbers %
                                                           abundant-numbers-for-problem)
           (range 1 28123))))

(solve)
