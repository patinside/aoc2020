(ns aoc2020.day11
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (let [seats-map (->> (str/split
                         (slurp "resources/day11")
                         #"\n")
                       (map #(str/split % #"")))]
    (->> (for [[i row] (map-indexed list seats-map)
               [j cell] (map-indexed list row)]
           {[i j] cell})
         (into {}))))

(defn get-adjacent-seats
  [seats-map [x y]]
  (->> (for [i (range -1 2)
             j (range -1 2)]
         {[(+ x i) (+ y j)] (get seats-map [(+ x i) (+ y j)])})
       (into {})
       (remove #(nil? (second %)))
       (remove #(= [x y] (first %)))
       (into {})))

(defn all-adjacent-seats-empty?
  [seats-map [x y]]
  (let [adjacent-seats (get-adjacent-seats seats-map [x y])
        adjacent-seats-availability (vals adjacent-seats)]
    (not (some #(= "#" %) adjacent-seats-availability))))

(defn at-least-4-adjacent-occupied-seats?
  [seats-map [x y]]
  (let [adjacent-seats (get-adjacent-seats seats-map [x y])
        adjacent-seats-availability (vals adjacent-seats)]
    (->> (filter #(= "#" %) adjacent-seats-availability)
         (count)
         (<= 4))))

(defn empty-seat?
  [seats-map [x y]]
  (= (get seats-map [x y]) "L"))

(defn occupied-seat?
  [seats-map [x y]]
  (= (get seats-map [x y]) "#"))

(defn swap-seat
  [seats-map seat]
  (let [x (first seat)
        y (second seat)]
    (if (and (all-adjacent-seats-empty? seats-map [x y])
             (empty-seat? seats-map [x y]))
      {[x y] "#"}
      (if (and (occupied-seat? seats-map [x y])
               (at-least-4-adjacent-occupied-seats? seats-map [x y]))
        {[x y] "L"}
        {[x y] (get seats-map [x y])}))))


(defn swap-seats
  [seats-map]
  (->> (map #(swap-seat seats-map (first %)) seats-map)
       (into {})))

(defn count-occupied-seats
  [seats-map]
  (->> seats-map
       (filter #(= "#" (second %)))
       (count)))

(defn solution-1
  [seats-map]
  (let [next-seats-map (swap-seats seats-map)]
    (if (= seats-map next-seats-map)
      (count-occupied-seats seats-map)
      (solution-1 next-seats-map))))

(defn build-txt-seats-map
  [seats-map]
  (->> (group-by #(first (first %)) (sort-by first seats-map))
       (sort-by first)
       (map second)
       (map #(map second %))
       (map #(reduce str %))
       (str/join "\n")))

(defn is-on-diagonal?
  [[x y] [x1 y1]]
  (let [x-diff (- x x1)
        y-diff (- y y1)]
    (= (Math/abs x-diff) (Math/abs y-diff))))

(defn is-on-line?
  [[x _] [x1 _]]
  (= x x1))

(defn is-on-column?
  [[_ y] [_ y1]]
  (= y y1))

(defn is-in-star-matrix?
  [a b]
  (or (is-on-diagonal? (first a) b)
      (is-on-line? (first a) b)
      (is-on-column? (first a) b)))

(defn all-usefull-seats-on-stars-matrix
  [seats-map [x y]]
  (->> seats-map
       (remove #(= (second %) "."))
       (into {})
       (filter #(is-in-star-matrix? % [x y]))
       (remove #(= [x y] (first %)))
       (into {})))

(defn get-nearest-top
  [seats-map [x y]]
  (->> seats-map
       (filter #(and
                  (< (first (first %)) x)
                  (= (second (first %)) y)))
       (sort-by #(- y (first (first %))))
       first))

(defn get-nearest-bottom
  [seats-map [x y]]
  (->> seats-map
       (filter #(and (> (first (first %)) x)
                     (= (second (first %)) y)))
       (sort-by #(- y (first (first %)) ))
       last))

(defn get-nearest-left
  [seats-map [x y]]
  (->> seats-map
       (filter #(and
                  (= (first (first %)) x)
                  (< (second (first %)) y)))
       (sort-by #(- y (second (first %))))
       first))

(defn get-nearest-right
  [seats-map [x y]]
  (->> seats-map
       (filter #(and
                  (= (first (first %)) x)
                  (> (second (first %)) y)))
       (sort-by #(- y (second (first %))))
       last))


(defn get-nearest-top-left
  [seats-map [x y]]
  (->> seats-map
       (filter #(and (< (first (first %)) x)
                     (< (second (first %)) y)))
       (sort-by first)
       last))

(defn get-nearest-top-right
  [seats-map [x y]]
  (->> seats-map
       (filter #(and (< (first (first %)) x)
                     (> (second (first %)) y)))
       (sort-by first)
       last))

(defn get-nearest-bottom-left
  [seats-map [x y]]
  (->> seats-map
       (filter #(and (> (first (first %)) x)
                     (< (second (first %)) y)))
       (sort-by first)
       first))

(defn get-nearest-bottom-right
  [seats-map [x y]]
  (->> seats-map
       (filter #(and (> (first (first %)) x)
                     (> (second (first %)) y)))
       (sort-by first)
       first))

(defn get-nearest
  [seat-map [x y]]
  (let [top (get-nearest-top seat-map [x y])
        bottom (get-nearest-bottom seat-map [x y])
        right (get-nearest-right seat-map [x y])
        left (get-nearest-left seat-map [x y])
        top-left (get-nearest-top-left seat-map [x y])
        top-right (get-nearest-top-right seat-map [x y])
        bottom-left (get-nearest-bottom-left seat-map [x y])
        bottom-right (get-nearest-bottom-right seat-map [x y])
        #__ #_( print "top" top "bottom" bottom "right" right "left" left "top-left" top-left "top-right" top-right "bottom-left" bottom-left "bottom-right" bottom-right "\n")]
    (->>  [top bottom right left top-left top-right bottom-left bottom-right]
          (remove #(nil? (second %)))
          (into {}))))

(defn at-least-5-adjacent-occupied-seats?
  [seats-map [x y]]
  (let [adjacent-seats (get-nearest seats-map [x y])
        adjacent-seats-availability (vals adjacent-seats)]
    (->> (filter #(= "#" %) adjacent-seats-availability)
         (count)
         (<= 5))))

(defn all-visible-seats-empty?
  [seats-map [x y]]
  (let [visible-seats (get-nearest seats-map [x y])
        visible-seats-availability (vals visible-seats)]
    (not (some #(= "#" %) visible-seats-availability))))

(defn swap-seat-1
  [seats-map seat]
  (let [x (first seat)
        y (second seat)
        available-seats (all-usefull-seats-on-stars-matrix seats-map [x y])]
    (if (and (all-visible-seats-empty? available-seats [x y])
             (empty-seat? seats-map [x y]))
      {[x y] "#"}
      (if (and (occupied-seat? seats-map [x y])
               (at-least-5-adjacent-occupied-seats? available-seats [x y]))
        {[x y] "L"}
        {[x y] (get seats-map [x y])}))))

(defn swap-seats-1
[seats-map]
(->> (map #(swap-seat-1 seats-map (first %)) seats-map)
     (into {})))

(defn count-occupied-seats
  [seats-map]
  (->> seats-map
       (filter #(= "#" (second %)))
       (count)))

(defn solution-2
  [seats-map]
  (let [next-seats-map (swap-seats-1 seats-map)
        _ (print (str(build-txt-seats-map next-seats-map) "\n----------------\n"))]
    (if (= seats-map next-seats-map)
      (count-occupied-seats seats-map)
      (solution-2 next-seats-map))))