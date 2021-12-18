(ns day18)

(def input (slurp "input18"))

(def sns (->> input (format "(%s)") read-string))

(defn paths+vals [sn]
  (lazy-seq
   (if (number? sn)
     [{:path nil :val sn}]
     (concat
      (map #(update % :path (partial into [0])) (paths+vals (first sn)))
      (map #(update % :path (partial into [1])) (paths+vals (second sn)))))))

(defn first-explodable [sn]
  (->> (concat [nil] (paths+vals sn) [nil])
       (partition 4 1)
       (filter #(-> % second :path count (= 5)))
       first))

(defn explode [sn
               [{left-path :path}
                {path :path left-val :val}
                {right-val :val}
                {right-path :path}]]
  (cond-> (assoc-in sn (subvec path 0 (dec (count path))) 0)
    left-path (update-in left-path + left-val)
    right-path (update-in right-path + right-val)))

(defn first-splittable [sn]
  (->> (paths+vals sn)
       (filter #(-> % :val (>= 10)))
       first))

(defn split [sn {:keys [path val]}]
  (assoc-in sn path [(quot val 2) (quot (inc val) 2)]))

(defn reduce-sn [sn]
  (or (some->> sn first-explodable (explode sn) reduce-sn)
      (some->> sn first-splittable (split sn) reduce-sn)
      sn))

(def sum-sn (comp reduce-sn vector))

(defn magnitude [sn]
  (if (number? sn)
    sn
    (+ (* 3 (magnitude (first sn)))
       (* 2 (magnitude (second sn))))))

;; part 1

(magnitude (reduce sum-sn sns))

;; part 2

(time
 (reduce max
         (for [x sns
               y sns]
           (magnitude (sum-sn x y)))))

;; "Elapsed time: 15563.462856 msecs" with a vector representation of path + val
;; "Elapsed time: 18719.101545 msecs" with map representation
