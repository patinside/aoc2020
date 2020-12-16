(ns aoc2020.day10
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (->> (str/split (slurp "resources/day10-test1") #"\n")
       (map #(Integer/parseInt %))
       (sort)))


(defn count-nb-n-diff
  [adapter-list n-diff]
  (let [adapt1 (butlast adapter-list)
        adapt2 (rest adapter-list)]
    (->> (map - adapt2 adapt1)
         (filter #(= % n-diff))
         count)))

(defn solution-1 []
  (let [in (conj (parse-in) 0)
        max-in (last in)
        my-device (+ max-in 3)
        in-plus-my-device (conj(vec in) my-device)
        nb-diff-1 (count-nb-n-diff in-plus-my-device 1)
        nb-diff-3 (count-nb-n-diff in-plus-my-device 3)]
    (* nb-diff-1 nb-diff-3)))

(defn consecutive?
  [coll y]
  (= y (inc (last coll))))


(defn consec-dist1-nb
  [coll n]
  (let [subset (drop n coll)]
    (reduce #(if  (consecutive? %1 %2)
               (conj %1 %2)
               (reduced %1)) [(first subset)] (rest subset))))

(defn partition-by-consecutive
  [coll]
  (->> (map  #(consec-dist1-nb coll %) (range (count coll)))
       (group-by last)
       (map #(first (second %)))
       (sort-by first)))

(defn count-path-list->tree
  ([tree-list]
   (let [node (first tree)
         children (take-while #(<= (- % node) 3) (drop 1 tree-list))
         _ (print tree-list)
         _ (print left-children middle-children right-children "\n")]
     (if (= (count children)0)
       1
       (+  (count-path-list->tree (drop 1 children))
          (count-path-list->tree ())
          (count-path-list->tree right-children))))))