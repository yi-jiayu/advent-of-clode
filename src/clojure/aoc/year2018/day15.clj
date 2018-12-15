(ns aoc.year2018.day15
  (:require [clojure.string :as string]
            [aoc.core :refer [adjacent]])
  (:import (clojure.lang PersistentQueue)))

(def starting-hp 200)

(defrecord BattleState [map units])

(defn enumerate2d
  "Returns a lazy sequence consisting of [row col elem] for each element in matrix."
  [matrix]
  (apply concat (map-indexed (fn [n row]
                               (map-indexed (fn [m val]
                                              [n m val]) row)) matrix)))

(defn parse-cavern-map
  [input]
  (->> input
       string/trim
       string/split-lines
       (mapv string/trim)))

(defn extract-units
  [cavern]
  (let [[elves goblins] (reduce (fn [[elves goblins] [row col c]]
                                  (case c
                                    \E [(assoc elves [row col] starting-hp) goblins]
                                    \G [elves (assoc goblins [row col] starting-hp)]
                                    [elves goblins]))
                                [{} {}]
                                (enumerate2d cavern))]
    {\E elves
     \G goblins}))

(defn initialise-battle-state
  [input]
  (let [map (parse-cavern-map input)
        units (extract-units map)]
    (->BattleState map units)))

(defn enemy-in-range
  "If a unit at `start-pos` is in range of at least one enemy unit represented
  by `target`, returns the position of the enemy unit in range with the lowest
  hit points."
  [battle start-pos target]
  (let [cavern (:map battle)
        enemy-positions (sort-by first (keep (fn [[pos tile]] (if (= target tile) pos))
                                             (for [adjacent (adjacent start-pos)]
                                               [adjacent (get-in cavern adjacent)])))]
    (if (not (empty? enemy-positions))
      (let [enemy-units (for [pos enemy-positions] [pos (get-in battle [:units target pos])])
            [pos _] (reduce (fn [[pos min-hp] [unit-pos unit-hp]] (if (< unit-hp min-hp)
                                                                    [unit-pos unit-hp]
                                                                    [pos min-hp]))
                            enemy-units)]
        pos))))

(defn find-target
  [cavern start-pos target]
  (let [choices (sort-by first (for [adjacent (adjacent start-pos)
                                     :when (= \. (get-in cavern adjacent))]
                                 [adjacent adjacent]))]
    (loop [open (apply conj PersistentQueue/EMPTY choices)
           closed #{}]
      (let [[choice curr] (peek open)
            open (pop open)
            adjacent (sort-by first (for [adjacent (adjacent curr)
                                          :when (not (closed adjacent))]
                                      [adjacent (get-in cavern adjacent)]))]
        (if (first (filter (fn [[_ tile]] (= target tile)) adjacent))
          [choice curr]
          (let [available-moves (keep (fn [[pos tile]] (if (= \. tile) [choice pos])) adjacent)]
            (recur (apply conj open available-moves)
                   (conj closed curr))))))))
