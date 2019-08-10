(ns clj-euler.thirty-six)

(defn is-base-ten-palindrome?
  "Is a number a palindrome in base 10?"
  [n]
  (= (str n) (->> n str reverse (apply str))))

(defn is-base-two-palindrome?
  "Is a number a palindrome in base 2?"
  [n]
  (let [base-two-n (Integer/toString n 2)]
    (= base-two-n
       (->> base-two-n reverse (apply str)))))

(defn is-base-two-and-ten-palindrome?
  "Is a number a palindrome in base 2 and base 10?"
  [n]
  (and (is-base-ten-palindrome? n)
       (is-base-two-palindrome? n)
       n))

(defn solve
  "Sum numbers less than one million which are palindromic in base 10 and base 2."
  []
  (apply
   +
   (filter
    is-base-two-and-ten-palindrome?
    (range 1 1000000))))

(solve)
