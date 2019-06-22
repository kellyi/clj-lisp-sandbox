(ns clj-euler.ten)

(defn multiple-of-three?
  "Is n a multiple of three?"
  [n]
  (-> n (rem 3) zero?))

(def not-multiple-of-three? (complement multiple-of-three?))

(defn multiple-of-five?
  "Is n a multiple of five?"
  [n]
  (-> n (rem 5) zero?))

(def not-multiple-of-five? (complement multiple-of-five?))

(defn sieve-of-e
  "Sieve of Eratosthones."
  ([n] (sieve-of-e (filter (every-pred odd?
                                       not-multiple-of-three?
                                       not-multiple-of-five?)
                           (range 5 n)) [2 3 5]))
  ([[h & tail] primes]
   (if (nil? h)
     primes
     (let [not-prime #(-> % (rem h) zero? not)]
       (recur (doall (filter not-prime tail)) (conj primes h))))))

(defn solve
  "Sum primes below n."
  [n]
  (->> n sieve-of-e (apply +)))

(solve 2000000)
