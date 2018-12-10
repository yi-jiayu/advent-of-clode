(ns aoc.year2017.day02
  (:require [clojure.string :refer [split split-lines]]))

(defn parse-row [row]
  (map #(Integer/parseInt %) (split row #"\s+")))

(defn parse-spreadsheet [input]
  (map parse-row (split-lines input)))

(defn line-checksum [line]
  (- (apply max line) (apply min line)))

(defn spreadsheet-checksum [spreadsheet]
  (apply + (map line-checksum spreadsheet)))

(defn find-result [line]
  (first (filter
          (every-pred integer? #(not= 1 %))
          (for [x line y line] (/ x y)))))

(defn sum-results [spreadsheet]
  (apply + (map find-result spreadsheet)))
