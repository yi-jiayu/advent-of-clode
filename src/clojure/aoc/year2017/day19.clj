(ns aoc.year2017.day19
  (:require [aoc.core :refer [vadd]]))

(def directions {:up    [-1 0]
                 :down  [1 0]
                 :left  [0 -1]
                 :right [0 1]})

(def turn-left {:up    :left
                :down  :right
                :left  :down
                :right :up})

(def turn-right {:left  :up
                 :right :down
                 :down  :left
                 :up    :right})

(defn load-routing-diagram
  [data]
  (clojure.string/split-lines data))

(defn find-start
  [diagram]
  [0 (.indexOf (first diagram) (int \|))])

(defn move
  [position direction]
  (vadd position (direction directions)))

(defn path?
  [x]
  (and (some? x) (not= x \space)))

(defn path-forward?
  [diagram position direction]
  (let [x (get-in diagram
                  (move position direction))]
    (path? x)))

(defn next-step
  [diagram position direction]
  (:pre (path? (get-in diagram position)))
  (cond
    (path-forward? diagram position direction) [(move position direction) direction]
    (path-forward? diagram position (turn-left direction)) [(move position (turn-left direction)) (turn-left direction)]
    (path-forward? diagram position (turn-right direction)) [(move position (turn-right direction)) (turn-right direction)]))

(defn letter?
  "Return true if x is a letter."
  [x]
  (<= 65 (int x) 90))

(defn follow
  [diagram]
  (let [start-pos (find-start diagram)]
    (loop [position start-pos
           direction :down
           letters []
           steps 0]
      (if (nil? position)
        [(apply str letters) steps]
        (let [letter (get-in diagram position)
              letters (if (letter? letter)
                        (conj letters letter)
                        letters)
              [position direction] (next-step diagram position direction)]
          (recur position
                 direction
                 letters
                 (inc steps)))))))
