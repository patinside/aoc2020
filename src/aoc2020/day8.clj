(ns aoc2020.day8
  (:require [clojure.string :as str]))

(defn inst-val->int
  [[inst value]]
  [inst (Integer/parseInt value)])

(defn parse-in
  []
  (->> (str/split (slurp "resources/day8") #"\n")
       (map #(str/split % #" "))
       (map inst-val->int)
       (zipmap (range))))

(defn process-instruction
  [[inst value] acc current-line]
  (case inst
    "acc" [(+ acc value) (+ 1 current-line)]
    "jmp" [acc (+ value current-line)]
    "nop" [acc (+ 1 current-line)]))

(defn process-instructions
  ([code]
   (process-instructions (get code 0) code 0 0 #{0}))
  ([[inst value] code acc line inst-acc]
   (let [[new-acc new-line] (process-instruction [inst value] acc line)
         [new-inst new-value] (get code new-line)]
     (if (contains? inst-acc new-line)
       ["infinite" new-acc]
       (if (= new-line 654)
         ["finit!!!" new-acc]
         (process-instructions [new-inst new-value] code new-acc new-line (conj inst-acc new-line)))))))

(defn solution-1
  []
  (->> (parse-in)
       (process-instructions)))


(defn inst-compl
  [inst]
  (case inst
    "nop" "jmp"
    "jmp" "nop"
    "acc" "acc"))

(defn altered-code
  [code [line-nb [inst value]]]
  (let [new-inst (inst-compl inst)
        new-duo [new-inst value]]
    (assoc code line-nb new-duo)))

(defn solution-2
  []
  (let [code (parse-in)]
    (->> (map process-instructions (map #(altered-code code %) code))
        (filter #(= (first %) "finit!!!")))))