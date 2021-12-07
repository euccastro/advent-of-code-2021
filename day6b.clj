(ns day6b)

(def example-input "3,4,3,1,2")
(def real-input (slurp "input6"))
(def input example-input)

(def initial-freqs
  (->> input
       (format "[%s]")
       read-string
       frequencies))

(defn step [freqs idx]
  (update freqs
          (mod (+ idx 7) 9)
          (fnil + 0 0)
          (get freqs (mod idx 9))))

(defn solution [days]
  (->>
   (reduce step initial-freqs (range days))
   vals
   (reduce +)))

;; part 1
(solution 80)
;; => 5934

;; part 2
(solution 256)
;; => 26984457539
