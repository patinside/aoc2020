(ns aoc2020.day12
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (->> (str/split-lines (slurp "resources/day12"))
       (map #(re-matches #"([FNRWSLE])(\d{1,3})" %))
       (map #(drop 1 %))
       (map #(conj [(first %)] (Integer/parseInt (second %))))))

(def counterclockwise-directions
  {"N" "E"
   "E" "S"
   "S" "W"
   "W" "N"})

(def anti-counterclockwise-directions
  {"E" "N"
   "S" "E"
   "W" "S"
   "N" "W"})

(defn turn-direction
  [orientation value direction]
  (let [next-direction (case orientation
                         "+" (get counterclockwise-directions direction)
                         "-" (get anti-counterclockwise-directions direction))
        new-value (- value 90)]
    (if (zero? new-value)
      next-direction
      (turn-direction orientation new-value next-direction))))

(defn change-status
  [[x y orientation] [inst value]]
  (let [value-to-return (case inst
                          "L" [x y (turn-direction "-" value orientation)]
                          "R" [x y (turn-direction "+" value orientation)]
                          "S" [x (- y value) orientation]
                          "E" [(+ x value) y orientation]
                          "F" (change-status [x y orientation] [orientation value])
                          "N" [x (+ y value) orientation]
                          "W" [(- x value) y orientation])
        _ (print value-to-return "\n")]
    value-to-return))

(defn solution
  ([]
   (let [consignes (parse-in)]
     (solution consignes [0 0 "E"])))
  ([consignes [x y orientation]]
   (if (empty? consignes)
     (+ (Math/abs x ) (Math/abs y ))
     (solution (rest consignes) (change-status [x y orientation] (first consignes))))))


(defn turn-waypoint
  [orientation [x y direction] value]
  (let [next-direction (case orientation
                         "+" (get counterclockwise-directions direction)
                         "-" (get anti-counterclockwise-directions direction))
        next-x (case orientation
                 "+" y
                 "-" (- y))
        next-y (case orientation
                 "+" (- x)
                 "-" x)
        new-value (- value 90)
        new-wp [ next-x next-y  next-direction]]
    (if (zero? new-value)
      new-wp
      (turn-direction orientation new-wp new-value))))

(defn change-status-1
  [[wp-x wp-y wp-direction] [boat-x boat-y] [inst value]]
  (let [value-to-return (case inst
                          "L" [[(turn-waypoint "-" [wp-x wp-y wp-direction] value)]
                               [boat-x boat-y ]]
                          "R" [(turn-waypoint "+" [wp-x wp-y wp-direction] value)
                               [boat-x boat-y]]
                          "S" [[wp-x (- wp-y value) wp-direction]
                               [boat-x boat-y ]]
                          "E" [[(+ wp-x value) wp-y wp-direction]
                               [boat-x boat-y ]]
                          "F" [[wp-x wp-y wp-direction] [(+ boat-x(* value wp-x))
                                                           (+ boat-y(* value wp-y))]]
                          "N" [[wp-x (+ wp-y value) wp-direction]
                               [boat-x boat-y ]]
                          "W" [[(- wp-x value) wp-y wp-direction]
                               [boat-x boat-y ]])
        _ (print inst value ": ")
        _ (print value-to-return "\n")]
    value-to-return))

(defn solution-2
  ([]
   (let [consignes (parse-in)]
     (solution-2 consignes [[10 1 "E"] [0 0]])))
  ([consignes [[wp-x wp-y wp-orientation] [boat-x boat-y boat-orientation]]]
   (if (empty? consignes)
     (+ (Math/abs boat-x) (Math/abs boat-y))
     (solution-2 (rest consignes) (change-status-1 [wp-x wp-y wp-orientation] [boat-x boat-y boat-orientation] (first consignes))))))