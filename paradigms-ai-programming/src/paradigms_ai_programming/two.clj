(ns paradigms-ai-programming.two
  (:require [clojure.spec.alpha :as spec]))

(def *simple-grammar*
  '((sentence (noun-phrase verb-phrase))
    (noun-phrase (article noun))
    (verb-phrase (verb noun-phrase))
    (article the a)
    (noun robot mouse ball table)
    (verb took saw liked lost ate)))

(def *grammar* *simple-grammar*)

(defn rule-lhs
  "The left-hand side of a rule"
  [[left & _tail]]
  left)

(defn rule-rhs
  "The right-hand side of a rule"
  [[_left & right]]
  right)

(defn get-rule
  "Finds the first rewrite rule from the grammar matching a key, k"
  [k grammar]
  (let [func (fn [[lhs & _tail]] (= k lhs))
        rule (first (filter func grammar))]
    rule))

(defn rewrites
  "Return a list of possible rewrites for this category"
  [category]
  (rule-rhs (get-rule category *grammar*)))

(defn generate
  "Generate a random sentence or phrase"
  [phrase]
  (cond
    (list? phrase) (mapcat generate phrase)
    (rewrites phrase) (-> phrase rewrites rand-nth generate)
    :else (list phrase)))

(generate 'sentence)
