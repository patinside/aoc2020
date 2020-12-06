(ns aoc2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-in
  []
  (str/split (slurp "resources/day6''") #"\n\n"))

(defn remove-return-in-groups
  [groups]
  (map #(str/replace % #"\n" "") groups))

(defn solution-1
  []
  (->> (parse-in)
       remove-return-in-groups
       (map #(into #{} %))
       (map count)
       (reduce +)))

(defn manage-group
  [group]
  (->> group
       (str/split-lines)
       (map #(into #{} %))
       (reduce set/intersection )
       count))


(defn solution-2
  []
  (->> (parse-in)
       (map manage-group)
       (reduce +)))

