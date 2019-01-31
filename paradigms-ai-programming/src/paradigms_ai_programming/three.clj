(ns paradigms-ai-programming.three
  (:require [clojure.spec.alpha :as spec]))

;; Write a version of `length` using the function `reduce`

(defn length
  "Find the length of a sequence"
  [s]
  (reduce (fn [acc, _next] (inc acc)) 0 s))

(length '(1 2 3))
(length "hello")
(length {:hello 'hello :world 'world})
(length #{1 2 3 4 5})
(length [])

;; Write an expression using `format` to take a list of words and print them in
;; a sentence, with the first word capitalized and a period after the last
;; word.

(defn my-format
  "Format a list of strings as a sentence"
  [[h & tail]]
  (format "%s %s." (clojure.string/capitalize h) (clojure.string/join " " tail)))

(my-format ["hello" "world"])
