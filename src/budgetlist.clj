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

(defn number-of-days-in-the-month [dt]
  (->> dt
       (t/last-day-of-the-month-)
       (t/day)))

(defn budget-by-day [day]
  (as-> day d
    (f/unparse yyyy-mm-formatter d)
    (get budget-pack d 0)
    (/ d (number-of-days-in-the-month day))))

(defn find-budget-between
  [from to]
  (->> (time-range from (t/plus to (t/days 1)) (t/days 1))
       (map budget-by-day)
       (reduce +)))

(defn -main []
  (print (find-budget-between (t/date-time 2018 2 1) (t/date-time 2018 4 6))))
