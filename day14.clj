(ns day14
  (:require [clojure.string :as str]))

(def input (slurp "input14"))

(let [[tmpl rules] (str/split input #"\R\R")]
  (def tmpl (->> tmpl
                 (format "|%s|")
                 (partition 2 1)
                 frequencies))
  (def rules (->> rules
                  str/split-lines
                  (map #(str/split % #" -> "))
                  (map (fn [[k v]] [(seq k) (first v)]))
                  (into {}))))

(defn step [m]
  (reduce
   (fn [acc [[begin end] n]]
     (let [middle (rules [begin end])]
       (cond-> acc
         middle
         (->
          (update [begin end] - n)
          (update [begin middle] (fnil + 0) n)
          (update [middle end] (fnil + 0) n)))))
   m
   m))

(defn solution [step-count]
  (->> tmpl
       (iterate step)
       (drop step-count)
       first
       (reduce
        (fn [acc [[begin end] n]]
          (-> acc
              (update begin (fnil + 0) n)
              (update end (fnil + 0) n)))
        {})
       (#(dissoc % \|))
       vals
       ((juxt (partial reduce max) (partial reduce min)))
       (apply -)
       (#(/ % 2))))

;; part 1
(solution 10)

;; part 2
(time
 (solution 40))
;; "Elapsed time: 14.013784 msecs"
;; => 3459174981021
