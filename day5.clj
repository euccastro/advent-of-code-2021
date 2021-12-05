(ns day5)

(def example-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def real-input (slurp "input5"))

(def input example-input)

(defn range-including [start end]
  (if (= start end)
    (repeat start)
    (let [step (compare end start)]
      (range start (+ end step) step))))

(defn num-overlaps [include-diagonals?]
  (->> input
       (format "[%s]")
       read-string
       (partition 5)
       (mapcat (fn [[x0 y0 _ x1 y1]]
                 (when (or include-diagonals? (= x0 x1) (= y0 y1))
                   (map vector
                        (range-including x0 x1)
                        (range-including y0 y1)))))
       frequencies
       vals
       (filter #(> % 1))
       count))

;; part 1
(num-overlaps false)
;; => 5

;; part 2
(num-overlaps true)
;; => 12
