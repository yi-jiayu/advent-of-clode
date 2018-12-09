(ns aoc.year2018.day09
  (:import (java.util LinkedList)))

(defn insert-marble-normal
  "Inserts the next marble into circle given the current index, returning the updated circle and the new current index."
  [^LinkedList circle current-index next-marble]
  (let [new-index (rem (+ current-index 2) (.size circle))
        new-index (if (zero? new-index) (.size circle) new-index)]
    (do (.add circle new-index next-marble) [circle new-index])))

(defn insert-marble-special
  "Inserts a marble with a value that is a multiple of 23 into circle."
  [^LinkedList circle current-index]
  (let [new-index (mod (- current-index 7) (.size circle))
        removed (.get circle new-index)]
    (do (.remove circle ^int new-index)
        [circle new-index removed])))

(defn update-score
  [scores player points]
  (let [current-score (get scores player 0)]
    (assoc scores player (+ current-score points))))

(defn play-marble-game
  [num-players last-marble]
  (loop [scores {}
         whose-turn 1
         circle (new LinkedList [0 1])
         current-index 1
         next-marble 2]
    (if (> next-marble last-marble)
      scores
      (if (zero? (rem next-marble 23))
        (let [[circle new-index removed] (insert-marble-special circle current-index)
              scores (update-score scores whose-turn (+ next-marble removed))]
          (recur scores
                 (rem (inc whose-turn) num-players)
                 circle
                 new-index
                 (inc next-marble)))
        (let [[circle new-index] (insert-marble-normal circle current-index next-marble)]
          (recur scores
                 (rem (inc whose-turn) num-players)
                 circle
                 new-index
                 (inc next-marble)))))))
