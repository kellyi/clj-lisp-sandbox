(ns fun-data-stuctures.two)


;; Exercise 2.1
;;
;; Write a function `suffixes` ... that takes a list `xs` and returns a list
;; of all the suffixes of `xs` in decreasing order of length.

(defn suffixes
  ([xs] (suffixes xs []))
  ([xs accumulator]
   (cond
     (empty? xs) (conj accumulator xs)
     :else (suffixes (rest xs) (conj accumulator xs)))))

(suffixes '(1 2 3 4))
