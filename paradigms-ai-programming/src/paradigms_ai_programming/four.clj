(ns paradigms-ai-programming.four
  (:require [clojure.spec.alpha :as spec]))

(declare appropriate-p achieve make-op apply-op GPS)

(def *state*
  "Global state atom for general problem solver"
  (atom nil))

(def *ops*
  "A set of operation maps"
  nil)

(spec/def ::action symbol?)
(spec/valid? ::action 'kelly)

(spec/def ::goal symbol?)
(spec/def ::goals set?)
(spec/def ::op
  (spec/keys :req-un [::action symbol?
                      ::preconds set?
                      ::add-list set?
                      ::del-list set?]))

(defn make-op
  "Create an operation map"
  [action preconds add-list del-list]
  {:pre [(spec/valid? symbol? action)
         (spec/valid? set? preconds)
         (spec/valid? set? add-list)
         (spec/valid? set? del-list)]}
  {:action action
   :preconds preconds
   :add-list add-list
   :del-list del-list})

(defn appropriate-p
  "An op is appropriate to a goal if it is in its add list"
  [goal op]
  {:pre [(spec/valid? ::goal goal)
         (spec/valid? ::op op)]}
  (contains? (:op-add-list op #{}) goal))

(defn achieve
  "A goal is achieved if it already holds of if there is an applicable appropriate op for it"
  [goal]
  {:pre [(spec/valid? ::goal goal)]}
  (or (contains? @*state* goal)
      (some apply-op
            (clojure.set/select (partial appropriate-p goal) *ops*))))

(defn apply-op
  "Print a message and update the *state* atom if op is applicable"
  [op]
  {:pre [(spec/valid? ::op op)]}
  (when (every? achieve (:preconds op))
    (do
      (println (str "executing " (:action op)))
      (reset! *state* (clojure.set/difference @*state* (:del-list op #{})))
      (reset! *state* (clojure.set/union @*state* (:add-list op #{})))
      true)))

(defn GPS
  "General Problem Solver: solve all goals using ops"
  [goals]
  {:pre [(spec/valid? ::goals goals)]}
  (cond
    (every? achieve goals) 'solved
    :else false))

