(ns paradigms-ai-programming.two
  (:require [clojure.spec.alpha :as spec]))

(def *simple-grammar*
  '((sentence (noun-phrase verb-phrase))
    (noun-phrase (article noun))
    (verb-phrase (verb noun-phrase))
    (article the a)
    (noun robot mouse ball table)
    (verb took saw liked lost ate)))

(def *bigger-grammar*
  '((sentence (noun-phrase verb-phrase))
    (noun-phrase (article adj* noun pp*) (person-name) (pronoun))
    (verb-phrase (verb noun-phrase pp*))
    (pp* () (pp pp*))
    (adj* () (adj adj*))
    (pp (prep noun-phrase))
    (prep to in by with on)
    (adj big little blue green)
    (article the a)
    (person-name pat kim lee terry robin)
    (noun robot mouse ball table)
    (verb high-fived saw liked lost ate)
    (pronoun he she it they those that)))

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

(defn generate-tree
  "Generate a random sentence or phrase with a complete parse tree"
  [phrase]
  (cond
    (list? phrase) (mapcat generate-tree phrase)
    (rewrites phrase) (cons phrase
                            (-> phrase rewrites rand-nth generate-tree))
    :else (list phrase)))

(generate-tree 'sentence)

(defn cross-product
  "Return a list of all (fun x y) values"
  [fun xlist ylist]
  (for [x xlist
        y ylist]
    (fun x y)))

(defn combine-all
  "Create all pairs from two lists"
  [xlist ylist]
  (cross-product list xlist ylist))

(combine-all '(A B) '(1 2))
(cross-product * '(1 2 3) '(10 20 30))

(defn generate-all
  "Generate a list of all possible expansions of this phrase"
  [phrase]
  (cond
    (nil? phrase) (list nil)
    (list? phrase) (combine-all (generate-all (first phrase))
                                (generate-all (rest phrase)))
    (rewrites phrase) (mapcat generate-all (rewrites phrase))
    :else (-> phrase list list)))

(generate-all 'article)
