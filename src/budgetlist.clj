(ns budgetlist
  (:require [clj-time.core :as t]
            [clj-time.periodic :as p]
            [clj-time.format :as f]))

(def yyyy-mm-formatter (f/formatter "yyyy-MM"))
(def budget-pack {"2018-02" 2800 "2018-04" 3000})

(defn time-range
  [start end step]
  (let [inf-range (p/periodic-seq start step)
        below-end? (fn [t] (t/within? (t/interval start end) t))]
    (take-while below-end? inf-range)))

(defn number-of-days-in-the-month
  [dt]
  (t/day (t/last-day-of-the-month- dt)))

(defn budget-by-day
  [day]
  (let [days (number-of-days-in-the-month day)]
    (/ (get budget-pack (f/unparse yyyy-mm-formatter day) 0) days)))

(defn find-budget-between
  [from to]
  (let [r (time-range from (t/plus to (t/days 1)) (t/days 1))]
    (reduce + (map budget-by-day r))))

(defn -main []
  (def from (t/date-time 2018 2 1))
  (def to (t/date-time 2018 4 6))
  (print (find-budget-between from to)))
