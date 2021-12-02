(ns day2
  (:require [clojure.string :as str]))

(def example-input "forward 5
down 5
forward 8
up 3
down 8
forward 2
")

(def real-input (slurp "input2"))

(defn input->pairs [input]
  (->> input
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[cmd n]] [cmd (read-string n)]))))

(defn solution1 [input]
  (->> input
       input->pairs
       (reduce
        (fn[[x depth] [cmd n]]
          (case cmd
            "up" [x (- depth n)]
            "down" [x (+ depth n)]
            "forward" [(+ x n) depth]))
        [0 0])
       (apply *)))

(defn solution2 [input]
  (->> input
       input->pairs
       (reduce
        (fn [[x depth aim] [cmd n]]
          (case cmd
            "up" [x depth (- aim n)]
            "down" [x depth (+ aim n)]
            "forward" [(+ x n) (+ depth (* aim n)) aim]))
        [0 0 0])
       butlast
       (apply *)))

(solution1 example-input)
;; => 150
(solution2 example-input)
;; => 900
