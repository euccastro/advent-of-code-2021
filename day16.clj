(ns day16)

(do
  (def input (slurp "input16"))
  (def bits
    (->> input
         (map {\0 "0000"
               \1 "0001"
               \2 "0010"
               \3 "0011"
               \4 "0100"
               \5 "0101"
               \6 "0110"
               \7 "0111"
               \8 "1000"
               \9 "1001"
               \A "1010"
               \B "1011"
               \C "1100"
               \D "1101"
               \E "1110"
               \F "1111"})
         (apply str))))

(defn subbit-val [bits start end]
  (-> bits (subs start end) (->> (str "2r")) read-string))

(defn packet-type [bits]
  (subbit-val bits 3 6))

(def bool->int {true 1 false 0})

(def ops {0 +
          1 *
          2 min
          3 max
          5 (comp bool->int >)
          6 (comp bool->int <)
          7 (comp bool->int =)})

(defn read-expr
  "[packet-repr rest-of-bits]"
  [bits]
  (if (every? #{\0} bits)
    nil
    (let [version (subbit-val bits 0 3)
          ptype (packet-type bits)]
      (if (= ptype 4)
        ;; literal
        (let [end-idx (->> (range 6 ##Inf 5)
                           (drop-while #(= (nth bits %) \1))
                           second)]
          [{:version version
            :type :literal
            :value (->> (subs bits 6 end-idx)
                        (partition 5)
                        (mapcat rest)
                        (apply str "2r")
                        read-string)}
           (subs bits end-idx)])
        (if (= (nth bits 6) \0)
          ;; operand length in bits
          (let [length (subbit-val bits 7 22)
                end (+ 22 length)]
            [{:version version
              :type :op
              :op (get ops ptype)
              :operands (->> [nil (subs bits 22 end)]
                             (iterate (comp read-expr second))
                             rest
                             (map first)
                             (take-while some?))}
             (subs bits end)])
          ;; operand length in packets
          (let [npackets (subbit-val bits 7 18)
                [operands remaining]
                (->> [nil (subs bits 18)]
                     (iterate (comp read-expr second))
                     rest
                     (take npackets)
                     ((juxt (partial map first) (comp second last))))]
            [{:version version
              :type :op
              :op (get ops ptype)
              :operands operands}
             remaining]))))))

;; part 1

(defn version-sum [ast]
  (reduce +
          (:version ast)
          (map version-sum (get ast :operands []))))

(-> bits read-expr first version-sum)

;; part 2

(defn eval-packet [ast]
  (if (= (:type ast) :literal)
    (:value ast)
    (apply (:op ast) (map eval-packet (:operands ast)))))

(-> bits read-expr first eval-packet)
