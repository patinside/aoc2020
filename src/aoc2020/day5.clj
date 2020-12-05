(ns aoc2020.day5
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (str/split (slurp "resources/day5") #"\n"))

(defn get-next-range
  [[current-min current-max] next-instructions]
  (let [next-instruction (first next-instructions)
        range-size (- current-max current-min)
        half-range-size (Math/floor (/ range-size 2.0))]
    (case next-instruction
      (\B \R) (get-next-range [(- current-max half-range-size) current-max] (rest next-instructions))
      (\F \L) (get-next-range [current-min (+ current-min half-range-size)] (rest next-instructions))
      nil (int current-min))))

(defn get-seat-id
  [line]
  (let [rows-instructions (take 7 line)
        row (get-next-range [0 127] rows-instructions)
        columns-instructions (drop 7 line)
        column (get-next-range [0 7] columns-instructions)
        seat-id (+(* row 8) column)]
    seat-id))

(defn solution-1
  []
  (->> (parse-in)
       (map get-seat-id)
       sort
       (last)))

(defn solution-2
  []
  (let [seats (->> (parse-in)
                   (map get-seat-id)
                   sort)
        next-seats (rest seats)
        seats-diff (map - next-seats seats)]
    (->>(zipmap seats seats-diff)
        (filter #(= 2 (second %)))
        (first)
        (first)
        (inc))))
