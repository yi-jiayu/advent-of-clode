(ns aoc.year2017.day14
  (:require [aoc.year2017.day10 :refer [knot-hash]])
  (:require [aoc.year2016.day13 :refer [count-set-bits]]))

(defn to-bin
  "Converts a hexadecimal hash digest into its binary representation."
  [hex]
  (apply str (map
               (comp #(clojure.string/replace % " " "0")
                     #(format "%4s" %)
                     #(Integer/toBinaryString %)
                     #(Character/digit ^Character % 16))
               hex)))

(defn to-grid
  [key-string]
  (pmap (comp knot-hash
              (partial str key-string "-"))
        (range 128)))

(defn used-squares
  [grid]
  (reduce + (map count-set-bits (flatten grid))))
