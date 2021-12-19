(ns day19
  (:require [clojure.core.matrix :as mat]
            [clojure.set :as set]
            [clojure.string :as str]))


(def input (slurp "input19"))

(defn exact [v]
  (mapv #(if (int? %) % (int (Math/round %))) v))

;; mat/det is only implemented in this one
(mat/set-current-implementation :vectorz)

(def rotations
  (for [xsign [1 -1]
        xpos (range 3)
        ysign [1 -1]
        ypos (range 3)
        zsign [1 -1]
        zpos (range 3)
        :let [m (mat/matrix
                 [(assoc [0 0 0] xpos xsign)
                  (assoc [0 0 0] ypos ysign)
                  (assoc [0 0 0] zpos zsign)])]
        :when (= (int (mat/det m)) 1)]
    (mapv exact m)))

(def scans
  (-> input
      (str/split #"\R\R")
      (->> (map str/split-lines)
           (map rest)
           (map (comp set
                      (partial map
                               #(->> %
                                     (format "[%s]")
                                     read-string)))))))

(defn offsets [scan1 scan2]
  (into #{}
   (for [beacon1 scan1
         beacon2 scan2]
     (mat/sub beacon1 beacon2))))

(defn assimilate-scan [acc scan]
  (first
   (for [r rotations
         :let [rotated-scan (map #(mat/mmul r %) scan)]
         o (offsets acc rotated-scan)
         :let [xf-scan (into #{} (map #(-> % (mat/add o) exact) rotated-scan))]
         :when (<= 12 (count (set/intersection xf-scan acc)))]
     [(set/union acc xf-scan) (mapv - (exact o))])))

(defn step [state]
  (reduce
   (fn [{:keys [acc unassimilated scanners] :as state} scan]
     (if-let [[new-acc scanner] (time (assimilate-scan acc scan))]
       {:acc new-acc
        :unassimilated (remove #{scan} unassimilated)
        :scanners (conj scanners scanner)}
       state))
   state
   (:unassimilated state)))

;; over 13 minutes with real input...
(def match
  (time
   (->> {:acc (first scans)
         :unassimilated (rest scans)
         :scanners #{[0 0]}}
        (iterate step)
        (drop-while (comp seq :unassimilated))
        first)))

;; part 1

(-> match :acc count)

;; part 2

(reduce max
        (for [s1 (:scanners match)
              s2 (:scanners match)]
          (reduce + (map (comp #(Math/abs %) -) s1 s2))))
