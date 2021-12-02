(ns day1
  (:require [clojure.string :as str]))

(def example-input "199
200
208
210
200
207
240
269
260
263")

(def real-input (slurp "input1"))

(defn input->numbers [input]
  (->> input
       str/split-lines
       (map read-string)))

(defn count-increasing [numbers]
  (->> numbers
       (partition 2 1)
       (filter (partial apply <))
       count))

(defn solution1 [input]
  (-> input
      input->numbers
      count-increasing))

(defn solution2 [input]
  (->> input
       input->numbers
       (partition 3 1)
       (map (partial apply +))
       count-increasing))

(solution1 example-input)
;; => 7
(solution2 example-input)
;; => 5
