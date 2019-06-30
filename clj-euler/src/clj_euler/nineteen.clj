(ns clj-euler.nineteen
  (:use [clj-time.core :as time]
        [clj-time.format :as time-format]
        [clj-time.predicates :only [sunday?]]))

(defn is-sunday?
  "Given a date, check if that date is a Sunday?"
  [date]
  (sunday? date))

(defn create-days-from-year-to-year
  "Given a start year and an end year create all days of those years."
  [start-year end-year]
  (for [year (range start-year (inc end-year))
        month (range 1 13)]
    (time/date-time year month 1)))

(defn solve
  "Count of Sundays that fell on the first of the month from start-year to end-year."
  [start-year end-year]
  (count
   (filter is-sunday? (create-days-from-year-to-year start-year end-year))))

(solve 1901 2000)
