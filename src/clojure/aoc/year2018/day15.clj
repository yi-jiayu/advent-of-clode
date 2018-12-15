(ns aoc.year2018.day15
  (:require [clojure.string :as string]
            [aoc.core :refer [adjacent]])
  (:import (clojure.lang PersistentQueue)))

(def starting-hp 200)

(defrecord BattleState [map elves goblins])

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

(defn extract-combatants
  [map]
  (reduce (fn [[elves goblins] [row col c]]
            (case c
              \E [(assoc elves [row col] starting-hp) goblins]
              \G [elves (assoc goblins [row col] starting-hp)]
              [elves goblins]))
          [{} {}]
          (enumerate2d map)))

(defn initialise-battle-state
  [input]
  (let [map (parse-cavern-map input)
        [elves goblins] (extract-combatants map)]
    (->BattleState map elves goblins)))

(defn find-target
  [cavern start-pos target]
  (loop [open (conj PersistentQueue/EMPTY start-pos)
         closed #{}]
    (let [curr (peek open)
          open (pop open)
          adjacent (sort-by first (for [adjacent (adjacent curr)
                                        :when (not (closed adjacent))]
                                    [adjacent (get-in cavern adjacent)]))]
      (if (first (filter #(= target (second %)) adjacent))
        curr
        (recur (apply conj open (keep (fn [[pos tile]] (if (= \. tile) pos)) adjacent))
               (conj closed curr))))))
