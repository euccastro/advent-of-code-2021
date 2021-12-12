(ns day12
  (:require [clojure.string :as str]))

(def input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn map-vals [f m]
  (into {}
        (for [[k v] m]
          [k (f v)])))

(def graph (->> input
                str/split-lines
                (map #(str/split % #"\-"))
                (mapcat (juxt identity reverse))
                (group-by first)
                (map-vals (partial mapv second))))

;; {"start" ["A" "b"], "A" ["c" "b" "end"], "b" ["d" "end"]}
