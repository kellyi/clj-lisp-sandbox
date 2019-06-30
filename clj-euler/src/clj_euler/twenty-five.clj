(ns clj-euler.twenty-five)

(declare find-nth-fibonacci-number)

(defn nth-fibonacci-number
  "Find the nth Fibonacci number."
  [n]
  (cond
    (= n 1) 1
    (= n 2) 1
    :else (+' (-> n dec find-nth-fibonacci-number)
              (-> n dec dec find-nth-fibonacci-number))))

(def find-nth-fibonacci-number
  "Memoized function to find the nth fibonacci number."
  (memoize nth-fibonacci-number))

(defn has-n-digits
  "Does a number x have n digits?"
  [n x]
  (= n (count (str x))))

(defn solve
  "Find the first Fibonacci number to have n digits."
  ([n] (solve n 1))
  ([n index]
   (if (has-n-digits n (find-nth-fibonacci-number index))
     index
     (recur n (inc index)))))

(solve 1000)
