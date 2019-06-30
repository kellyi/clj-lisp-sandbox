(ns clj-euler.twenty-one)

(defn find-all-divisors
  "Find all divisors of n."
  [n]
  (reduce (fn [acc x]
            (if (->> x (rem n) zero?)
              (conj acc x (quot n x))
              acc))
          [1]
          (range 2 (Math/sqrt n))))

(defn sum-divisors
  "Sum all proper divisors of n."
  [n]
  (apply + (find-all-divisors n)))

(defn is-amicable-number?
  "Is n an amicable number?"
  [n]
  (cond
    (-> n sum-divisors (= n)) nil
    (->> n sum-divisors sum-divisors (= n)) n
    :else nil))

(defn solve
  "Sum the amicable numbers below n."
  [n]
  (apply + (filter is-amicable-number? (range 1 n))))

(solve 10000)
