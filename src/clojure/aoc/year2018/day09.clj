(ns aoc.year2018.day09
  (:import (aoc.year2018.day09 Node)))

(defn insert-marble-normal
  "Inserts the next marble into circle."
  [circle next-marble]
  (let [circle (.insertAfter (. circle next) next-marble)]
    circle))

(defn seek-backward
  "Move `n` steps backwards in `circle`."
  [circle n]
  (if (zero? n)
    circle
    (nth (iterate #(. % prev) circle) n)))

(defn insert-marble-special
  "Inserts a marble with a value that is a multiple of 23 into circle, returning the value of the removed marble."
  [circle]
  (let [circle (seek-backward circle 6)
        removed (.removeBefore circle)]
    [circle removed]))

(defn update-score
  [scores player points]
  (let [current-score (get scores player 0)]
    (assoc scores player (+ current-score points))))

(defn play-marble-game
  [num-players last-marble]
  (loop [scores {}
         whose-turn 0
         circle (new Node 0)
         next-marble 1]
    (if (> next-marble last-marble)
      scores
      (if (zero? (rem next-marble 23))
        (let [[circle removed] (insert-marble-special circle)
              scores (update-score scores whose-turn (+ next-marble removed))]
          (recur scores
                 (rem (inc whose-turn) num-players)
                 circle
                 (inc next-marble)))
        (let [circle (insert-marble-normal circle next-marble)]
          (recur scores
                 (rem (inc whose-turn) num-players)
                 circle
                 (inc next-marble)))))))
