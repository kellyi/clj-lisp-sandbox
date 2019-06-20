(ns clj-euler.three)

(defn prime?
  "Is a number prime?"
  [n]
  (and (> n 1)
       (not-any? (partial (fn
                            [x y]
                            (zero? (mod x y)))
                          n)
                 (range 2 n))))

(defn collect-prime-factors
  "Collect the prime factors of a number n."
  ([n]
   (if (prime? n)
     '(1)
     (collect-prime-factors n 2 '(1))))
  ([n next-factor factors]
   (cond
     (prime? n) (cons n factors)
     (zero? (mod n next-factor)) (recur (/ n next-factor)
                                        next-factor
                                        (cons next-factor factors))
     :else (recur n (inc next-factor) factors))))

(defn solve
  "Find the largest prime factor of a number n."
  [n]
  (->> n collect-prime-factors (apply max)))

(solve 600851475143)
