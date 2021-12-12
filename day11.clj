(ns day11
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp "input11"))

(defn mapvv [f xs]
  (mapv (partial mapv f) xs))

(def initial-energy (->> input str/split-lines (mapvv (comp parse-long str))))

(def row-count (count initial-energy))
(def col-count (count (first initial-energy)))

(defn within-bounds? [[y x]]
  (and (< -1 y row-count)
       (< -1 x col-count)))

(defn cell-neighbors [cell]
  (->> cell
       (map (juxt dec identity inc))
       (apply combo/cartesian-product)
       (remove #{cell})
       (filter within-bounds?)))

(def inc-all (partial mapvv inc))

(defn bursting [nrg flashed]
  (for [[y row] (map-indexed vector nrg)
        [x v] (map-indexed vector row)
        :when (and (> v 9) (not (flashed [y x])))]
    [y x]))

(defn flash-one [nrg cell]
  (reduce
   (fn [nrg' cell']
     (update-in nrg' cell' inc))
   nrg
   (cell-neighbors cell)))

(defn flash [nrg flashed]
  (let [b (into #{} (bursting nrg flashed))]
    (if (seq b)
      (recur
       (reduce flash-one nrg b)
       (set/union flashed b))
      [(reduce (fn [nrg' pos]
                 (assoc-in nrg' pos 0))
               nrg
               flashed)
       flashed])))

(defn step [[nrg acc]]
  (let [[nrg' flashed] (flash (inc-all nrg) #{})]
    [nrg' (+ acc (count flashed))]))

;; part 1
(second (nth (iterate step [initial-energy 0]) 100))

;; part 2
(->> (iterate step [initial-energy 0])
     (map-indexed vector)
     (drop-while (fn [[_idx [nrg _acc]]]
                   (some #(some (complement #{0}) %)
                         nrg)))
     ffirst)
