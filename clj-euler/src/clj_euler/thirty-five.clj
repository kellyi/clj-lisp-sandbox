(ns clj-euler.thirty-five)

(defn is-prime?
  "Is a number prime?"
  [n]
  (and (> n 1)
       (not-any? (fn
                   [y]
                   (zero? (mod n y)))
                 (range 2 (Math/sqrt (inc n))))))

(def prime?
  "Memoized function to check whether a number is prime."
  (memoize is-prime?))

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  ;; https://clojure.github.io/clojure-contrib/seq-api.html#clojure.contrib.seq/rotations
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(defn all-rotations-are-prime?
  "Are all rotations of a number prime?"
  [n]
  (every?
   prime?
   (map
    #(->> % (apply str) Integer/parseInt)
    (-> n str rotations))))

(defn solve
  "Count all circular primes below limit."
  [limit]
  (->> limit
       (range 2)
       (filter prime?)
       (filter all-rotations-are-prime?)
       count))

(solve 1000000)
