(ns aoc.year2018.day03
  (:require [clojure.math.combinatorics :refer [selections]]))

(defn parse-claim
  [input]
  (some->> input
           (clojure.string/trim)
           (re-matches #"#(?<id>\d+) @ (\d+),(\d+): (\d+)x(\d+)")
           (rest)
           (map #(Integer/parseInt %))
           (zipmap [:id :col-offset :row-offset :width :height])))

(defn parse-claims
  [input]
  (map parse-claim (-> input
                       (clojure.string/trim)
                       (clojure.string/split-lines))))

(defn make-cloth
  [n]
  (into [] (repeat n
                   (into [] (repeat n 0)))))

(defn should-update?
  [offset length index]
  (< (- offset 1)
     index
     (+ offset length)))

(defn should-update-column?
  [claim index]
  (let [{offset :col-offset
         length :width} claim]
    (should-update? offset length index)))

(defn should-update-row?
  [claim index]
  (let [{offset :row-offset
         length :height} claim]
    (should-update? offset length index)))

(defn update-column
  [claim index value]
  (if (should-update-column? claim index)
    (inc value)
    value))

(defn update-row
  [claim index row]
  (if (should-update-row? claim index)
    (into [] (map-indexed (partial update-column claim) row))
    row))

(defn mark-claim
  [cloth claim]
  (into [] (map-indexed (partial update-row claim) cloth)))

(defn mark-claims
  [cloth claims]
  (reduce mark-claim cloth claims))

(defn count-disputed
  [cloth]
  (count (filter (partial < 1) (flatten cloth))))

(defn edges
  [claim]
  (let [top (:row-offset claim)
        bot (+ top (:height claim) -1)
        left (:col-offset claim)
        right (+ left (:width claim) -1)]
    [top bot left right]))

(defn intersects?
  [claim1 claim2]
  (let [id1 (:id claim1)
        id2 (:id claim2)
        [top1 bot1 left1 right1] (edges claim1)
        [top2 bot2 left2 right2] (edges claim2)]
    (not (or (= id1 id2)
             (> left2 right1)
             (< right2 left1)
             (> top2 bot1)
             (< bot2 top1)))))

(defn intersects-none?
  [claims claim]
  (every? false? (map (partial intersects? claim) claims)))

(defn find-non-overlapping-claim
  [claims]
  (first (filter (partial intersects-none? claims) claims)))
