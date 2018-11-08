(ns aoc.year2015.day02
  (:require [clojure.string :refer [split split-lines]]))

(defn parse-lwh [s]
  (map #(Integer/parseInt %) (split s #"x")))

(defn parse-input [input]
  (let [lines (split-lines input)]
    (map parse-lwh lines)))

(defn surface-area [lwh]
  (let [[l w h] lwh]
    (* 2 (+ (* l w) (* w h) (* h l)))))

(defn smallest-side [lwh]
  (let [[l w h] lwh]
    (min (* l w) (* w h) (* h l))))

(defn wrapping-paper-required [lwh]
  (+ (surface-area lwh) (smallest-side lwh)))

(defn total-wrapping-paper-required [presents]
  (apply + (map wrapping-paper-required presents)))
