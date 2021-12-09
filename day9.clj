(ns day9
  (:require [clojure.string :as str]))

(def input (slurp "input9"))

(def heightmap (->> input str/split-lines (mapv #(mapv (comp parse-long str) %))))

(def height #(get-in heightmap (vec %)))

(def neighbor-deltas
  [[1 0] [-1 0] [0 1] [0 -1]])

(defn neighbors-by-higherness [coords]
  (let [h (height coords)]
    (->> neighbor-deltas
         (map (partial map + coords))
         (filter height)
         (group-by #(-> % height (> h) ({true :higher false :not-higher}))))))

(def low-points
  (for [[y line] (map-indexed vector heightmap)
        [x depth] (map-indexed vector line)
        :when (not (:not-higher (neighbors-by-higherness [y x])))]
    [y x depth]))

;; part 1
(reduce + (map (comp inc last) low-points))

;; part 2

(defn set-pop [s]
  (-> s first ((juxt identity (partial disj s)))))

(defn get-basin-size [low-point]
  (loop [frontier #{low-point}
         visited #{}
         sz 0]
    (if (empty? frontier)
      sz
      (let [[p frontier] (set-pop frontier)]
        (if (visited p)
          (recur frontier visited sz)
          (recur (->> p
                      neighbors-by-higherness
                      :higher
                      (filter #(not= (height %) 9))
                      (apply conj frontier))
                 (conj visited p)
                 (inc sz)))))))

(apply * (->> low-points (map butlast) (map get-basin-size) sort reverse (take 3)))
