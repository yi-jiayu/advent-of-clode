(ns aoc.year2018.day18
  (:require [aoc.core :refer [vadd enumerate2d]]))

(defn parse-input
  [input]
  (->> input
       clojure.string/trim
       clojure.string/split-lines
       (mapv (partial into []))))

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
  ([area]
   (iterate next-state-for-area area))
  ([area n]
   (nth (transduce-area area) n)))

(defn find-repetition
  [f x0]
  (loop [tortoise (f x0)
         hare (f (f x0))]
    (if (= tortoise hare)
      tortoise
      (recur (f tortoise)
             (f (f hare))))))

(defn find-mu
  [f x0 hare]
  (loop [mu 0
         tortoise x0
         hare hare]
    (if (= tortoise hare)
      [mu tortoise]
      (recur (inc mu)
             (f tortoise)
             (f hare)))))

(defn find-lambda
  [f tortoise]
  (loop [lambda 1
         hare (f tortoise)]
    (if (= tortoise hare)
      lambda
      (recur (inc lambda)
             (f hare)))))

(defn floyd
  [f x0]
  (let [hare (find-repetition f x0)
        [mu tortoise] (find-mu f x0 hare)
        lambda (find-lambda f tortoise)]
    [mu lambda]))

(defn find-first-repetition-and-cycle-time
  [area]
  (floyd next-state-for-area area))

(defn transduce-optimised
  "Returns the state of area after `n` time steps for large n by first finding
  a cycle."
  [area n]
  (let [[first-repetition cycle-time] (find-first-repetition-and-cycle-time area)]
    (if (< n first-repetition)
      (transduce-area area n)
      (let [parity (rem (- n first-repetition) cycle-time)]
        (transduce-area area (+ first-repetition parity))))))
