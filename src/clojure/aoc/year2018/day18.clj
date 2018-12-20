(ns aoc.year2018.day18
  (:require [aoc.core :refer [vadd enumerate2d]]))

(defn surrounding-tiles
  "Returns the coordinates of the 8 tiles surrounding `position`."
  [position]
  (map (partial vadd position) [[-1 -1] [-1 0] [-1 1]
                                [0 -1] [0 1]
                                [1 -1] [1 0] [1 1]]))

(defn count-surrounding-tile-states
  "Counts the amount of each type of tile surrounding `position`."
  [area position]
  (frequencies (keep (partial get-in area) (surrounding-tiles position))))

(defn next-state-for-tile
  "Returns the next state for tile given its surroundings."
  [tile surroundings]
  (case tile
    \. (if (>= (surroundings \| 0) 3) \| \.)
    \| (if (>= (surroundings \# 0) 3) \# \|)
    \# (if (and (>= (surroundings \# 0) 1)
                (>= (surroundings \| 0) 1)) \# \.)))

(defn next-state-for-area
  "Returns the next state for an entire area."
  [area]
  (reduce (fn [area' [row col tile]]
            (let [position [row col]]
              (assoc-in area' position (->> position
                                            (count-surrounding-tile-states area)
                                            (next-state-for-tile tile)))))
          area
          (enumerate2d area)))

(defn calculate-resource-value
  "Calculates the total resource value for `area`."
  [area]
  (let [counts (frequencies (flatten area))]
    (* (counts \| 0) (counts \# 0))))

(defn transduce-area
  "Returns the state of an area after `n` minutes."
  [n area]
  (nth (iterate next-state-for-area area) n))

(defn parse-input
  [input]
  (->> input
       clojure.string/trim
       clojure.string/split-lines
       (mapv (partial into []))))
