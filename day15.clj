(ns day15
  (:require [clojure.data.priority-map :as pm]
            [clojure.string :as str]))

(def input (slurp "input15"))

(defn mapvv [f xs]
  (mapv (partial mapv f) xs))

(def cave (->> input str/split-lines (mapvv (comp parse-long str))))
(def cave-height (count cave))
(def cave-width (count (first cave)))
(def goal (mapv dec [cave-height cave-width]))

(def pos-cost (partial get-in cave))

(defn h [[y x]]
  (+ cave-height (- y) cave-width (- x)))

(defn neighbors [[y x]]
  (for [[dy dx] [[1 0] [-1 0] [0 1] [0 -1]]
        :let [y' (+ y dy)
              x' (+ x dx)]
        :when (and (< -1 y' cave-height)
                   (< -1 x' cave-width))]
    [y' x']))

(def initial-search-state {:visited #{[0 0]}
                           :frontier (pm/priority-map {:pos [0 0] :cost 0}
                                                      0)})

(defn step [{:keys [visited frontier]}]
  (let [[{:keys [pos cost]}] (peek frontier)]
    (if (= pos goal)
      {:solution cost}
      {:visited (conj visited pos)
       :frontier (into (pop frontier)
                       (for [pos' (neighbors pos)
                             :when (not (visited pos'))
                             :let [cost' (+ cost (pos-cost pos'))]]
                         [{:pos pos' :cost cost'} (+ cost' (h pos'))]))})))

(-> initial-search-state :frontier peek)
(defn solution []
  (->> initial-search-state
       (iterate step)
       (drop-while (complement :solution))
       first
       :solution))

(solution)

;; part 2

(def part1-cave-width cave-width)
(def part1-cave-height cave-height)

(def cave-width (* cave-width 5))
(def cave-height (* cave-height 5))
(def goal (mapv dec [cave-height cave-width]))

(defn pos-cost [[y x]]
  (->
   (+ (get-in cave [(mod y part1-cave-height) (mod x part1-cave-width)])
      (quot y part1-cave-height)
      (quot x part1-cave-width))
   dec
   (mod 9)
   inc))

(time
 (solution))
;; "Elapsed time: 3883.446463 msecs"
;; => 2925
