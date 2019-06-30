(ns clj-euler.seventeen
  (:use [com.gfredericks.forty-two :only [words]]))

(defn remove-non-alphanumeric-chars
  "Remove all non-alphanumeric chars from a string."
  [s]
  (->>
   s
   (filter #(Character/isLetter %))
   (apply str)))

(defn solve
  "Sum the chars of the first n numbers."
  [n]
  (->>
   n
   inc
   (range 1)
   (map (comp count remove-non-alphanumeric-chars words))
   (apply +')))

(solve 1000)
