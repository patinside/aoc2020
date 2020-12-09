(ns aoc2020.day9
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (->> (str/split (slurp "resources/day9") #"\n")
       (map #(biginteger %))))

(defn sum-all-pairs
  [coll]
  (->> (for [a coll
             b coll]
         (when (not= a b)
           (+ a b)))
       (remove nil?)))

(defn validate-number
  [prev-n-numbers candidate]
  (let [possibilities (sum-all-pairs prev-n-numbers)]
    (some #( = candidate %) possibilities)))

(defn validate-serie
  [serie n]
  (let [ prev-n-numbers (take n serie)
        candidate (nth serie n)]
    (if (validate-number prev-n-numbers candidate)
      (validate-serie (rest serie) n)
      candidate)))

(defn solution-1
  []
  (-> (parse-in)
      (validate-serie 25)))

(defn find-contiguous-n-numbers-equal-candidate
  [serie n candidate]
  (if (>= (count serie) n)
    (let [n-numbers (take n serie)]
      (if (= (reduce + n-numbers) candidate)
        n-numbers
        (find-contiguous-n-numbers-equal-candidate (rest serie) n candidate)))
    false))

(defn scan-n-contiguous-numbers
  ([serie candidate]
   (scan-n-contiguous-numbers serie 2 candidate))
  ([serie n candidate]
   (let [potential-solution (find-contiguous-n-numbers-equal-candidate serie n candidate)]
     (or potential-solution (scan-n-contiguous-numbers serie (inc n) candidate)))))

(defn solution-2
  [] (let [serie (parse-in)
           candidate (solution-1)
           solution-serie (scan-n-contiguous-numbers serie candidate)
           sorted-solution-serie (sort solution-serie)
           min (first sorted-solution-serie)
           max (last sorted-solution-serie)]
    (+ min max)))