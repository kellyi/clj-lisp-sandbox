(ns clj-euler.twenty-two)

(def input
  (as-> "src/clj_euler/twenty-two.input.txt"
    data
    (slurp data)
    (clojure.string/split data #",")
    (map clojure.edn/read-string data)
    (sort data)))

(def alphabet-map
  "Map chars of the alphabet to their positions."
  (zipmap (map (comp str char) (range 97 123)) (range 1 27)))

(defn sum-word
  "Given a word, sum the positions of its digits."
  [word]
  (map #(->> % str clojure.string/lower-case (get alphabet-map)) word))

(defn solve
  "Sum all letters in a list, multiplying them by their indices."
  []
  (apply + (map-indexed (fn [index word]
                          (* (inc index)
                             (apply + (sum-word word))))
                        input)))

(solve)
