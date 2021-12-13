(ns day13
  (:require [clojure.string :as str]))

(def input (slurp "input13"))

(def x 0)
(def y 1)

(let [[points fold-instructions] (str/split input #"\R\R")]
  (def paper
    (->> points
         str/split-lines
         (map #(->> % (format "[%s]") read-string))))
  (def fold-instructions
    (->> fold-instructions
         (re-seq #"fold along ([xy])=(\d+)")
         (map rest)
         (map (partial map (comp eval read-string))))))

(defn fold-point [p [coord fold-v]]
  (update p coord #(let [diff (- % fold-v)]
                     (if (pos? diff)
                       (- fold-v diff)
                       %))))

(defn fold-paper [paper fold-instruction]
  (->> paper (map #(fold-point % fold-instruction)) (into #{})))

;; part1

(-> paper (fold-paper (first fold-instructions)) count)

;; part2
(def folded (reduce fold-paper paper fold-instructions))
(def width (inc (reduce max (map first folded))))
(def height (inc (reduce max (map second folded))))
(doseq [y (range height)]
  (println
   (apply str (for [x (range width)]
                (if (folded [x y]) "#" " ")))))
