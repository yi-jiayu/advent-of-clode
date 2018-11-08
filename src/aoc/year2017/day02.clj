(ns aoc.year2017.day02
    (:require [clojure.string :refer [split split-lines]]))

(defn parse-row [row]
  (map #(Integer/parseInt %) (split row #"\s+")))

(defn parse-spreadsheet [input]
  (map parse-row (split-lines input)))

(defn find-result [line]
  (first (filter
          (every-pred integer? #(not= 1 %))
          (for [x line y line] (/ x y)))))

(defn sum-results [spreadsheet]
  (apply + (map find-result spreadsheet)))
