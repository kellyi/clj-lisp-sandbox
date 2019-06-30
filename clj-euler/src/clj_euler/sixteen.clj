(ns clj-euler.sixteen)

(defn pow
  "Calculate n to the x."
  [n x]
  (if (zero? x)
    1
    (*' n (pow n (dec x)))))

(defn sum-digits
  "Sum the digits of a number n."
  ([n] (sum-digits n 0))
  ([n acc]
   (if (< n 10)
     (+ n acc)
     (recur (quot n 10) (+ acc (rem n 10))))))

(defn solve
  "Sum the digits of 2 raised to the nth power."
  [nth]
  (->> nth (pow 2) sum-digits))

(solve 1000)
