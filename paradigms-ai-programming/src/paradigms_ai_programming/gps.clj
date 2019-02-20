(ns paradigms-ai-programming.gps
  (:require [clojure.spec.alpha :as spec]))

(declare achieve apply-op appropriate-p)

(def state
  "The current state; a list of conditions"
  (atom #{}))

(def ops
  "A list of available operators"
  (atom #{}))

(def find-all
  "Alias for PAIP find-all operation"
  filter)

(defn make-op
  "Create an operation"
  [action preconds add-list del-list]
  {:action action
   :preconds preconds
   :add-list add-list
   :del-list del-list})

(def school-ops
  #{(make-op :drive-son-to-school
             #{:son-at-home :car-works}
             #{:son-at-school}
             #{:son-at-home})
    (make-op :shop-installs-battery
             #{:car-needs-battery :shop-knows-problem :shop-has-money}
             #{:car-works}
             #{})
    (make-op :tell-shop-problem
             #{:in-communication-with-shop}
             #{:shop-knows-problem}
             #{})
    (make-op :telephone-shop
             #{:know-phone-number}
             #{:in-communication-with-shop}
             #{})
    (make-op :look-up-number
             #{:have-phone-book}
             #{:know-phone-number}
             #{})
    (make-op :give-shop-money
             #{:have-money}
             #{:shop-has-money}
             #{:have-money})
    })

(defn appropriate-p
  "An op is appropriate to a goal if it is in its add list."
  [goal op]
  (contains? (:add-list op) goal))

(defn achieve
  "A goal is achieved if it already holds or if there is an appropriate op for it that is applicable"
  [goal]
  (or (contains? @state goal)
      (some apply-op
            (find-all (fn [op] (appropriate-p goal op)) @ops))))

(defn apply-op
  "Print a message and update state if op is applicable"
  [op]
  (when (every? achieve (:preconds op))
    (println (str "executiing op " (:action op)))
    (swap! state #(clojure.set/difference % (:del-list op)))
    (swap! state #(clojure.set/union % (:add-list op)))
    true))

(defn GPS
  "General problem solved: achieve all goals using ops"
  [goals new-state new-ops]
  (do
    (reset! state new-state)
    (reset! ops new-ops)
    (if (every? achieve goals) :solved)))

(GPS #{:son-at-school}
     #{:son-at-home :car-needs-battery :have-money :have-phone-book}
     school-ops)
