(ns clj-euler.twelve)

(defn is-prime?
  "Is a number prime?"
  [n]
  (and (> n 1)
       (not-any? (fn
                   [y]
                   (zero? (mod n y)))
                 (range 2 (inc (Math/sqrt n))))))

(def prime?
  "Memoized function to check whether a number is prime."
  (memoize is-prime?))

(defn collect-prime-factors
  "Collect the prime factors of a number n."
  ([n]
   (if (prime? n)
     '()
     (collect-prime-factors n 2 '())))
  ([n next-factor factors]
   (cond
     (prime? n) (cons n factors)
     (zero? (mod n next-factor)) (recur (/ n next-factor)
                                        next-factor
                                        (cons next-factor factors))
     :else (recur n (inc next-factor) factors))))

(defn generate-nth-triangle-number
  "Generate the nth triangle number."
  ([nth] (generate-nth-triangle-number nth 1))
  ([nth counter]
   (if (= nth 1)
     counter
     (recur (dec nth) (+ nth counter)))))

(def gen-nth-triangle-number
  "Memoized function to generate the nth triangle number."
  (memoize generate-nth-triangle-number))

(defn find-divisor-count
  "Find the count of divisors of n."
  [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (->>
           n
           collect-prime-factors
           frequencies
           vals
           (map #(inc %))
           (apply *))))

(defn solve
  "Find the first triangle number to have more than n divisors"
  ([n] (solve n 1))
  ([divisors counter]
   (let [triangle-number (gen-nth-triangle-number counter)
         divisor-count (find-divisor-count triangle-number)]
     (if (> divisor-count divisors)
       triangle-number
       (recur divisors (inc counter))))))

(solve 500)
