(ns day12
  (:require [clojure.string :as str]))

(def input (slurp "input12"))

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

(defn count-paths [node visited]
  (if (= node "end")
    1
    (let [visited (cond-> visited
                    (not= node (str/upper-case node)) (conj node))]
      (reduce +
              0
              (map #(count-paths % visited)
                   (remove visited (graph node)))))))

(count-paths "start" #{})

(defn count-paths2 [node visited allow-revisit?]
  (if (= node "end")
    1
    (let [small? (not= node (str/upper-case node))
          allow-revisit? (and allow-revisit?
                              (not (and small? (visited node))))
          visited (cond-> visited
                    small? (conj node))]
      (reduce +
              0
              (map #(count-paths2 % visited allow-revisit?)
                   (cond->>
                       (remove #{"start"} (graph node))
                     (not allow-revisit?) (remove visited)))))))

(count-paths2 "start" #{} true)
;; => 146553
