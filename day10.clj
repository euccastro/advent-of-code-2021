(ns day10
  (:require [clojure.string :as str]))

(def input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def input (slurp "input10"))

(def closing {\{ \}
              \[ \]
              \( \)
              \< \>})

(defn step [acc c]
  (let [cl (closing c)]
    (cond
      cl (cons cl acc)
      (= c (first acc)) (rest acc)
      :else (reduced c))))

;; part 1

(def score {\) 3
            \] 57
            \} 1197
            \> 25137})

(defn line-score [line]
  (let [res (reduce step nil line)]
    (if (char? res) (score res) 0)))

(->> input str/split-lines (map line-score) (reduce +))

;; part 2

(def score2 {\) 1
             \] 2
             \} 3
             \> 4})

(defn line-score2 [line]
  (let [res (reduce step nil line)]
    (when-not (char? res)
      (reduce
       (fn [acc n]
         (+ (* acc 5) (score2 n)))
       0
       res))))

(->> input
     str/split-lines
     (map line-score2)
     (remove nil?)
     sort
     (#(nth % (quot (count %) 2))))
