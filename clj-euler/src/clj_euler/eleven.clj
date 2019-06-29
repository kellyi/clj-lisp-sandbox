(ns clj-euler.eleven)

(def input
  (as-> "src/clj_euler/eleven.input.txt"
    data
   (slurp data)
   (clojure.string/split data #"\n")
   (map (fn [row]
          (map (fn [col]
                 (Integer/parseInt col))
               (clojure.string/split row #" ")))
        data)))

(defn fours
  "Given a list return tails of four elements."
  ([l] (fours l '()))
  ([l acc]
   (if (< (count l) 4)
     acc
     (recur (drop 1 l) (cons (take 4 l) acc)))))

(defn find-max-horizontal-value
  "Find the max horizontal value of multiplying 4 adjacent numbers in a matrix m."
  [m]
  (apply max (map (fn [row]
                    (->> row fours (map #(apply * %)) (apply max)))
                  m)))

(defn find-max-vertical-value
  "Find the max vertical value of multiplying 4 adjacent numbers in a matrix m."
  [m]
  (find-max-horizontal-value (apply map list m)))

(defn find-max-right-diagonal-value
  "Find the max right diagonal value of multiplying 4 adjacent numbers in a matrix m."
  [m]
  (let [col-indices (filter
                     #(= (count %) 4)
                     (for [x (range 0 (count m))] (range x (min (+ 4 x) (count m)))))
        row-indices (map (fn [row-index]
                           [row-index
                            (inc row-index)
                            (-> row-index inc inc)
                            (-> row-index inc inc inc)])
                         (range 0 (- (count m) 3)))
        all-indices (for [row row-indices
                          col col-indices]
                      (zipmap row col))
        vals (map (fn [combo]
                    (map (fn [pair]
                           (let [row (key pair)
                                 col (val pair)]
                             (nth (nth m row) col)))
                         combo))
                  all-indices)]
    (apply max (map #(apply * %) vals))))

(defn find-max-left-diagonal-value
  "Find the max left diagonal value of multiplying 4 adjacent numbers in a matrix m."
  [m]
  (let [col-indices (filter
                     #(= (count %) 4)
                     (map (fn [col-index]
                            [col-index
                             (dec col-index)
                             (-> col-index dec dec)
                             (-> col-index dec dec dec)])
                          (range 3 (count m))))
        row-indices (map (fn [row-index]
                           [row-index
                            (inc row-index)
                            (-> row-index inc inc)
                            (-> row-index inc inc inc)])
                         (range 0 (- (count m) 3)))
        all-indices (for [row row-indices
                          col col-indices]
                      (zipmap row col))
        vals (map (fn [combo]
                    (map (fn [pair]
                           (let [row (key pair)
                                 col (val pair)]
                             (nth (nth m row) col)))
                         combo))
                  all-indices)]
    (apply max (map #(apply * %) vals))))

(defn solve
  "Find the maximum product of four adjacent numbers in an array."
  []
  (max (find-max-vertical-value input)
       (find-max-horizontal-value input)
       (find-max-right-diagonal-value input)
       (find-max-left-diagonal-value input)))

(solve)
