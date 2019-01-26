(ns paradigms-ai-programming.two
  (:require [clojure.spec.alpha :as spec]))

(defonce articles '("the" "a"))
(defonce nouns '("cat" "ball" "table" "dog" "robot" "ghost" "ennui"))
(defonce verbs '("took" "saw" "ate" "haunted" "lost"))

(defn one-of
  "Select one random element from an input list"
  [l]
  (rand-nth l))

(defn noun-phrase
  "Generate a random noun phrase"
  []
  (list (one-of articles) (one-of nouns)))

(defn verb-phrase
  "Generate a rnadom verb phrase"
  []
  (concat (-> verbs one-of list) (noun-phrase)))

(defn sentence
  "Generate a random sentence"
  []
  (concat (noun-phrase) (verb-phrase)))

(sentence)
