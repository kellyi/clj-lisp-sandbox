(ns clj-euler.fourteen)

(defn next-collatz-number
  "Given n find the next Collatz number."
  [n]
  (cond
    (zero? n) nil
    (= n 1) nil
    (even? n) (/ n 2)
    :else (->> n (* 3) inc)))

(def find-next-collatz-number
  "Memoized function to find the next Collatz number."
  (memoize next-collatz-number))

(defn collatz-sequence
  "Given n find its Collatz sequence."
  ([n] (collatz-sequence n [n]))
  ([n accumulator]
   (let [next-number (find-next-collatz-number n)]
     (if (nil? next-number)
       accumulator
       (recur next-number (conj accumulator next-number))))))

(def find-collatz-sequence
  "Memoized function to find a number's Collatz sequence."
  (memoize collatz-sequence))

(defn collatz-sequence-count
  "Find the count of the Collatz sequence for a number n."
  [n]
  (-> n find-collatz-sequence count))

(def find-collatz-sequence-count
  "Memoized function to find the count of a number's Collatz sequence."
  (memoize collatz-sequence-count))

(defn solve
  "Find the number with the longest Collatz sequence beneath n."
  [n]
  (first (apply max-key
                second
                (map (fn [x]
                       [x (find-collatz-sequence-count x)])
                     (range 1 n)))))

(solve 1000000)
