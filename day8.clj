(ns day8
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def input (slurp "input8"))

(def lines (-> input str/split-lines))

;; part 1

(def easy-segment-counts #{2 4 3 7})

(->> lines
     (map #(str/split % #" \| "))
     (map second)
     (mapcat #(str/split % #"\s"))
     (map count)
     (filter easy-segment-counts)
     count)

;; part 2

(defn map-keys [f m]
  (->> m
       (map (fn [[k v]] [(f k) v]))
       (into {})))

(def segments->digit
  (map-keys set
            {"abcefg" "0"
             "cf" "1"
             "acdeg" "2"
             "acdfg" "3"
             "bcdf" "4"
             "abdfg" "5"
             "abdefg" "6"
             "acf" "7"
             "abcdefg" "8"
             "abcdfg" "9"}))

(def all-segments "abcdefg")

(defn line->number [line]
  (let [[samples out] (map #(str/split % #" ") (str/split line #" \| "))]
    (first
     (for [perm (combo/permutations all-segments)
           :let [m (zipmap perm all-segments)
                 sample->digit
                 (->> samples
                      (map (juxt set
                                 #(->> %
                                       (replace m)
                                       set
                                       segments->digit)))
                      (into {}))]
           :when (every? some? (vals sample->digit))]
       (->> out (map set) (map sample->digit) (apply str) parse-long)))))

(->> lines (map line->number) (reduce +))
