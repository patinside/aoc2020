(ns aoc2020.day4
  (:require [clojure.string :as str]))

(defn parse-in
  []
  (str/split (slurp "resources/day4") #"\n\n"))

(def required-field
  ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"] )

(defn parse-passport
       [passport-string]
       (->> (str/split  passport-string #"[ \n]")
            (remove empty?)
            (map #(str/split % #":"))
            (into {})))

(defn passport-contains-keys
  [parsed-passport]
  (->> (for [key required-field] (contains? parsed-passport key))
       (reduce #(and %1 %2) true)))

(defn solution-1
  []
  (->> (map parse-passport (parse-in))
       (filter passport-contains-keys )
       count))

(defn valid-byr?
  [parsed-passport]
  (let [byr-string (get parsed-passport "byr")
        byr-int (Integer/parseInt byr-string)]
    (if (and
          (= (count byr-string) 4)
          (<= 1920 byr-int 2002))
      true
      false)))

(defn valid-iyr?
  [parsed-passport]
  (let [iyr-string (get parsed-passport "iyr")
        iyr-int (Integer/parseInt iyr-string)]
    (if (and
          (= (count iyr-string) 4)
          (<= 2010 iyr-int 2020))
      true
      false)))

(defn  valid-eyr?
  [parsed-passport]
  (let [eyr-string (get parsed-passport "eyr")
        eyr-int (Integer/parseInt eyr-string)]
    (if (and
          (= (count eyr-string) 4)
          (<= 2020 eyr-int 2030))
      true
      false)))

(defn valid-hgt?
  [parsed-passport]
  (let [hgt-string (get parsed-passport "hgt")
        in-parse (re-matches #"(\d\d)in" hgt-string)
        cm-parse (re-matches #"(\d\d\d)cm" hgt-string)]
    (or
      (when (not (nil? in-parse)) (<= 59 (Integer/parseInt(nth in-parse 1)) 76))
      (when (not (nil? cm-parse)) (<= 150 (Integer/parseInt(nth cm-parse 1)) 193))
      false)))

(defn valid-hcl?
  [parsed-passport]
  (let [hcl-string (get parsed-passport "hcl")]
    (if (re-matches #"#[0-9a-f]{6}" hcl-string)
      true
      false)))

(defn valid-ecl?
  [parsed-passport]
  (let [ecl-string (get parsed-passport "ecl")
        ]
    (if (re-matches #"amb|blu|brn|gry|grn|hzl|oth" ecl-string)
      true
      false)))

(defn valid-pid?
  [parsed-passport]
  (let [pid-string (get parsed-passport "pid")
        ]
    (if (re-matches #"\d{9}" pid-string)
      true
      false)))

(def fn-mapping
  {"byr" valid-byr?
   "iyr" valid-iyr?
   "eyr" valid-eyr?
   "hgt" valid-hgt?
   "hcl" valid-hcl?
   "ecl" valid-ecl?
   "pid" valid-pid?
   })

(defn validation-fn
  [key]
  (get fn-mapping key))

(defn passport-contains-valid-keys?
  [parsed-passport]
  (->> (for [key required-field]
         ((validation-fn key) parsed-passport))
       (reduce #(and %1 %2) true)))


(defn solution-2
  []
  (->> (map parse-passport(parse-in))
       (filter passport-contains-keys)
       (filter passport-contains-valid-keys?)
       count))

