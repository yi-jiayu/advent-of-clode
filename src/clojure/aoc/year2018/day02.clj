(ns aoc.year2018.day02
  (:require [clojure.math.combinatorics :refer [cartesian-product]]))

(defn parse-box-ids
  [input]
  (-> input
      (clojure.string/trim)
      (clojure.string/split-lines)))

(defn contains-exactly-n-of-any-char?
  "Returns true if `s` contains exactly `n` of any char."
  [n s]
  (let [fs (frequencies s)]
    (some (partial = n) (vals fs))))

(defn checksum
  [box-ids]
  (let [twos (count (filter (partial contains-exactly-n-of-any-char? 2) box-ids))
        threes (count (filter (partial contains-exactly-n-of-any-char? 3) box-ids))]
    (* twos threes)))

(defn hamming-distance
  "Returns the Hamming distance between `s1` and `s2`."
  [s1 s2]
  (count (filter false? (map = s1 s2))))

(defn common-chars
  [s1 s2]
  (apply str
         (filter some?
                 (map #(if (= %1 %2) %1) s1 s2))))

(defn correct-box-ids
  "Returns the pair of box ids which differ by exactly one character."
  [box-ids]
  (first (drop-while #(not= 1 (apply hamming-distance %))
                     (cartesian-product box-ids box-ids))))

(defn common-chars-between-correct-boxes
  "Returns the letters that are common between the two correct box IDs in `box-ids`."
  [box-ids]
  (->> box-ids
       (correct-box-ids)
       (apply common-chars)))
