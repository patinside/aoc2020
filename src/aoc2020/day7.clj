(ns aoc2020.day7
  (:require [clojure.string :as str]))

(defn format-values
  [values]
  (->> (str/split values #", | \d|\d |\.")
       (remove #(= (count %) 0))
       (remove #(= % "no other bag"))))

  (defn format-line
    [[k v]]
    {k (format-values v)})

(defn parse-in
  []
  (->> (str/split (slurp "resources/day7") #"\n")
       (map #(str/replace % "bags" "bag"))
       (map #(str/split % #" contain "))
       (map format-line)))

(defn line-contains?
  [line children]
  (some (partial = children) (first (map second line))))

(defn find-parent
  [children coll]
  (->> (filter #(line-contains? % children) coll)
       (map #(first (first %)))))

(defn find-parents-of-parents
  ([coll child]
   (find-parents-of-parents coll [child] [] ))
  ([coll children acc]
   (let [next-parents-list (reduce #(into %1 (find-parent %2 coll)) [] children)]
     (if (empty? next-parents-list)
       acc
       (find-parents-of-parents coll next-parents-list (into acc next-parents-list))))))

(defn solution-1
  []
  (count (into #{} (find-parents-of-parents (parse-in) "shiny gold bag"))))



(defn value->map
  [value]
  ( let [split (re-matches #"(\d) (.*)" value)
         qty (Integer/parseInt(nth split 1))
         label (nth split 2)]
    {label qty}))

(defn values->map
  [values]
  (->> values
       (map value->map)
       (into {})))

(defn format-values-1
  [values]
  (->> (str/split values #", |\.")
       (remove #(= (count %) 0))
       (remove #(= % "no other bag"))
       values->map))

(defn format-line-1
  [[k v]]
  (let [formated-value (format-values-1 v)] {k formated-value}))

(defn parse-in-1
  []
  (->> (str/split (slurp "resources/day7") #"\n")
       (map #(str/replace % "bags" "bag"))
       (map #(str/split % #" contain "))
       (map format-line-1)
       (map first)
       (into {})))

(defn get-children
  [coll parent coef]
  (let [children-values (get coll parent)
        final-values (map #(* % coef) (vals children-values))]
    [(reduce-kv (fn [a k v] (assoc a k (* v coef))) {} children-values) (reduce + final-values)]))

(defn get-next-children
  ([coll parents som]
   (reduce-kv (fn [acc bag som]
                (let [[children value] (get-children coll bag som)
                      update-children (update acc :children #(merge-with + % children))
                      final-children (update update-children :acc #(+ value %))]
                  final-children)) {:children {} :acc som} parents)))

(defn solution-2
  ([coll parents total]
   (let [{:keys [children acc]} (get-next-children coll parents total)]
    (if (empty? parents)
      total
      (solution-2 coll children acc)))))