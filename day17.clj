(ns day17)

;; small input, didn't bother with parsing
(def xmin 20)
(def xmax 30)
(def ymin -10)
(def ymax -5)

;; part 1

(defn sum-to [n]
  (/ (* n (inc n)) 2))

(sum-to (dec (- ymin)))
;; => 45

;; part 2

(defn hits-target?
  [x y vx vy]
  (cond
    (and (<= xmin x xmax) (<= ymin y ymax)) true
    (or (< y ymin) (> x xmax)) false
    :else (recur (+ x vx)
                 (+ y vy)
                 (+ vx (compare 0 vx))
                 (dec vy))))

(def solutions
  (for [vx (range (inc xmax))
        vy (range ymin (- ymin))
        :when (hits-target? 0 0 vx vy)]
    [vx vy]))

(count solutions)
;; => 112
