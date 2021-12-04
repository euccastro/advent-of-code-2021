(ns day3
  (:require [clojure.string :as str]))

(def example-input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def real-input (slurp "input3"))
(def input real-input)
(def lines (str/split-lines input))

(defn bits->long [xs]
  (read-string (apply str "2r" xs)))

;; part 1

(->> lines
     (apply map vector)
     (map (partial group-by identity))
     (map (partial sort-by (comp count second)))
     (map ffirst)
     ((juxt identity (partial map {\0 \1 \1 \0})))
     (map bits->long)
     (apply *))

;; => 198

;; part 2

(defn indicator [which]
  (->> [lines 0]
       (iterate
        (fn [[remaining-lines idx]]
          [(->> remaining-lines
                (group-by #(nth % idx))
                vals
                sort
                ((case which
                   :co2-scrubber first
                   :oxygen-generator last)))
           (inc idx)]))
       (drop-while #(-> % first count (> 1)))
       ffirst
       bits->long))

(->> [:co2-scrubber :oxygen-generator]
     (map indicator)
     (apply *))

;; => 230
