(ns day17)

;; small input, didn't bother with parsing
(def xmin 20)
(def xmax 30)
(def ymin -10)
(def ymax -5)

;; part 1

(defn sum-to [n]
  (/ (* n (inc n)) 2))

(defn abs [x]
  (Math/abs x))

(def farthest-y
  (max-key abs ymin ymax))

(sum-to (cond-> (abs farthest-y)
          (neg? farthest-y) dec))
;; => 45

;; part 2

(def farthest-x
  (max-key abs xmin xmax))

(defn abs-range [x]
  (->> x
       abs
       ((juxt - inc))
       (apply range)))

(defn hits-target?
  ([vx vy]
   (hits-target? 0 0 vx vy))
  ([x y vx vy]
   (cond
     (and (<= xmin x xmax) (<= ymin y ymax)) true
     (or (< y ymin)
         (and (not (neg? vx)) (> x xmax))
         (and (not (pos? vx)) (< x xmin))) false
     :else (recur (+ x vx)
                  (+ y vy)
                  (+ vx (compare 0 vx))
                  (dec vy)))))

(reduce
 +
 (for [vx (abs-range farthest-x)
       vy (abs-range farthest-y)
       :when (hits-target? vx vy)]
   1))
