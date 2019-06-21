(ns clj-euler.seven)

(defn prime?
  "Is a number prime?"
  [n]
  (and (> n 1)
       (not-any? (fn
                   [y]
                   (zero? (mod n y)))
                 (range 2 n))))

(defn find-subsequent-prime
  "Given a number n, find its first subsequent prime."
  ([n] (find-subsequent-prime n (inc n)))
  ([n next]
   (if (prime? next)
     next
     (recur n (inc next)))))

(defn solve
  "Find the nth prime number"
  ([n]
   (if (= n 1)
     2
     (solve (dec n) 3)))
  ([n prime-counter]
   (cond
     (= n 1) prime-counter
     :else (recur (dec n)
                  (find-subsequent-prime prime-counter)))))

(solve 10001)
