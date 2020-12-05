(ns aoc2020.day1
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (map #(Integer/parseInt %)(str/split-lines (slurp "resources/day1-1"))))

(defn get-pair [nb-coll]
  (rest
    (first
     (filter #(= (first %) 2020)
             (for [a nb-coll
                   b nb-coll]
               [(+ a b) a b])))))

(defn get-triplet [nb-coll]
  (rest
    (first
         (filter #(= (first %) 2020)
                 (for [a nb-coll
                       b nb-coll
                       c nb-coll]
                   [(+ a b c) a b c])))))
(defn solution-1
  []
  (let [in (parse-in)
        result (get-pair in)]
    (apply * result)))

(defn solution-2
  []
  (let [in (parse-in)
        result (get-triplet in)]
    (apply * result)))