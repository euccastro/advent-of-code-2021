(ns day6)

(def example-input "3,4,3,1,2")
(def real-input (slurp "input6"))
(def input example-input)

(def initial-population (->> input (format "[%s]") read-string))

(def improper-descendants-after-days
  (memoize
   (fn [days age]
     (cond
       (= days 0) 1
       (= age 0) (+ (improper-descendants-after-days (dec days) 6)
                    (improper-descendants-after-days (dec days) 8))
       :else (improper-descendants-after-days (dec days) (dec age))))))

(defn solution [days]
  (reduce + (map (partial improper-descendants-after-days days) initial-population)))

;; part 1
(solution 80)
;; => 5934

;; part 2
(solution 256)
;; => 26984457539
