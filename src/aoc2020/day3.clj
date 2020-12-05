(ns aoc2020.day3
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (str/split-lines (slurp "resources/day3")))


(defn add-tree?
  [line index]
  (=
    (if (>= index (count line))
     (nth line (mod index (count line)) )
      (nth line index))
    \#))


(defn process-line
  [inc-index [current-index nb-trees line-number] line ]
  (let [next-index (+ current-index inc-index)
        inc-val (if(add-tree? line current-index) 1 0)
        next-nb-trees (+ nb-trees inc-val)
        next-line-number (inc line-number)]
    (print [current-index nb-trees line-number] line "\n")
    [next-index next-nb-trees next-line-number]))

(defn skip-line
  [acc]
  [(nth acc 0) (nth acc 1) (inc (nth acc 2))])

(defn manage-line
  [[h-inc v-inc] acc line]
  (let [line-number (nth acc 2)]
    (if (= (mod line-number v-inc) 0)
      (process-line h-inc acc line)
      (skip-line acc))))

(defn solution-1
  [coord data]
  (reduce  #(manage-line coord %1 %2) [(first coord) 0 1] (rest data)))

(defn solution-2
  [data]
  (map second
       (for
         [coord [[1 1] [3 1] [5 1] [7 1] [1 2]]]
         (solution-1 coord data))))