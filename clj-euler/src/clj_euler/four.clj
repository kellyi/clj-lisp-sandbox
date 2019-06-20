(ns clj-euler.four)

(defonce two-digit-products
  (for [x (range 10 100)
        y (range 10 100)]
    (* x y)))

(defonce three-digit-products
  (for [x (range 100 999)
        y (range 100 999)]
    (* x y)))

(defn get-number-digits
  "Get the digits of a number x."
  ([x] (get-number-digits x []))
  ([x number-digits]
   (if (< x 10)
     (conj number-digits x)
     (recur (quot x 10)
            (conj number-digits (mod x 10))))))

(defn is-palindrome-number?
  "Is a number n a palindrome?"
  ([n] (is-palindrome-number? n (get-number-digits n)))
  ([n number-digits]
   (cond
     (or (zero? (count number-digits)) (= 1 (count number-digits))) n
     (not= (first number-digits) (last number-digits)) false
     :else (recur n (drop 1 (drop-last number-digits))))))

(defn solve
  "Find the largest palindrome product of two 3-digit numbers."
  []
  (->>
   three-digit-products
   (filter is-palindrome-number?)
   (apply max)))

(solve)
