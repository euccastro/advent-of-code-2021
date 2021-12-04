(ns day4
  (:require [clojure.string :as str]))

(defn parse-num-line [s]
  (->> s
       (format "[%s]")
       read-string))

(defn transpose [xs]
  (apply map vector xs))

(def example-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def real-input (slurp "input4"))

(def input example-input)

;; part 1

(let [[draws & cards] (str/split input #"\R\R")]
  (def draws (parse-num-line draws))
  (def initial-cards
    (->> cards
         (map str/split-lines)
         (map (partial map parse-num-line)))))

(defn bingo-line? [xs]
  (every? nil? xs))

(defn bingo-rows? [card]
  (some bingo-line? card))

(defn bingo-card? [card]
  (some bingo-rows? [card (transpose card)]))

(defn mark [card n]
  (map
   (partial replace {n nil})
   card))

(defn sum-unmarked [tree]
  (apply + (filter some? (flatten tree))))

(defn reduce-step [cards draw]
  (let [new-cards (map #(mark % draw) cards)
        winning-card (first (filter bingo-card? new-cards))]
    (if winning-card
      (reduced (* draw (sum-unmarked winning-card)))
      new-cards)))

(reduce reduce-step initial-cards draws)
;; => 4512

;; part 2

(defn reduce-step2 [cards draw]
  (let [marked-cards (map #(mark % draw) cards)
        new-cards (remove bingo-card? marked-cards)]
    (if (seq new-cards)
      new-cards
      (reduced (* draw (sum-unmarked marked-cards))))))

(reduce reduce-step2 initial-cards draws)
;; => 1924
