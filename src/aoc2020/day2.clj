(ns aoc2020.day2
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (map #(str/split % #" ") (str/split-lines (slurp "resources/day2")) ))

(defn extract-range
  [line]
  (map #(Integer/parseInt %) (str/split (first line) #"-")))

(defn in-range?
  [range number]
  (>= (second range) number (first range)))

(defn valid-line-1?
  [line]
  (let [range (extract-range line)
        letter (first(nth line 1))
        psw (nth line 2)]
    (->> (filter #(= letter %) psw)
         count
         (in-range? range))))

(def solution-1
  (->>
    (map valid-line-1? (parse-in))
    (filter true?)
    count))

(defn xor
  [a b]
  (or (and a (not b)) (and b (not a))))

(defn valid-line-2?
  [line]
  (let [range (extract-range line)
        letter (first(nth line 1))
        psw (nth line 2)]
    (xor (= (nth psw (- (first range) 1)) letter)
         (= (nth psw (- (second range) 1)) letter))))

(def solution-2
  (->>
    (map valid-line-2? (parse-in))
    (filter true?)
    count))