(ns day7)

(def example-input "16,1,2,0,4,2,7,1,2,14")
(def real-input (slurp "input7"))
(def input real-input)

(def initial-positions (->> input (format "[%s]") read-string))

(defn min-cost [cost-fn]
  (->> initial-positions
       (map (fn [n]
              (reduce
               (fn [cost pos]
                 (+ cost (cost-fn (Math/abs (- pos n)))))
               0
               initial-positions)))
       (reduce min)))

;; part1

(time (min-cost identity))
;; "Elapsed time: 2187.234532 msecs"
;; => 357353

;; part2

;; Thank you, 6-year-old Gauss!
(time (min-cost #(/ (* % (inc %)) 2)))
;; "Elapsed time: 2229.310717 msecs"
;; => 104822130


;; slightly optimized; hardly worth the trouble

(def initial-position-freqs (frequencies initial-positions))

(defn min-cost' [cost-fn]
  (->> initial-position-freqs
       (map (fn [[pos pos-count]]
              (* pos-count
                 (reduce
                  (fn [cost [pos' pos-count']]
                    (+ cost (* pos-count' (cost-fn (Math/abs (- pos' pos))))))
                  0
                  initial-position-freqs))))
       (reduce min)))

(time (min-cost' identity))
;; "Elapsed time: 929.02989 msecs"

(time (min-cost' #(/ (* % (inc %)) 2)))
;; "Elapsed time: 965.869357 msecs"
