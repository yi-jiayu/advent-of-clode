(ns aoc.year2017.day11
  (:require [clojure.string :refer [split]]))

(def direction-map
  (hash-map :n [0 1] :s [0 -1] :ne [1 0] :sw [-1 0] :nw [-1 1] :se [1 -1]))

(defn add-pair [[x y] [dx dy]]
  (vector (+ x dx) (+ y dy)))

(defn move [pos direction]
  (add-pair pos (direction-map direction)))

(defn walk [steps]
  (reduce move [0 0] steps))

(defn distance
  "Returns the Manhattan distance from the origin to point (x, y) on a hexgrid."
  [[x y]]
  (let [z (- 0 x y)]
    (max (Math/abs x) (Math/abs y) (Math/abs z))))

(defn parse-input [input]
  (map keyword (split input #",")))

(defn fewest-steps-to-reach [path]
  (distance (walk path)))

(defn max-distance
  "Returns the furthest Manhattan distance from the origin reached along `path`."
  [path]
  (second (reduce
           (fn [[xy max-dist] direction]
             (let [xy (move xy direction)
                   dist (distance xy)
                   max-dist (if (> dist max-dist) dist max-dist)]
               (vector xy max-dist)))
           [[0 0] 0]
           path)))
