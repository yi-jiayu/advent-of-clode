(ns aoc.year2017.day13
  (:require [clojure.string :refer [split split-lines]]))

(defn initialise-firewall
  "Creates a firewall from its text representation."
  [input]
  (let [lines (map #(split % #": ") (split-lines input))]
    (map (fn [[depth range]] [(Integer/parseInt depth) (Integer/parseInt range)])
         lines)))

(defn scanner-position
  "Calculate the scanner position in a layer with range `r` at time `t`.
  The scanner starts at position 0 at time 0."
  [r t]
  (let [r' (- r 1)
        positions (into [] (concat (range r') (range r' 0 -1)))]
    (get positions (rem t (* 2 r')))))

(defn severity
  [[depth range]]
  (if (= 0 (scanner-position range depth))
    (* range depth)
    0))

(defn total-severity
  "Calculates the total severity of a trip through a firewall."
  [firewall]
  (apply + (map severity firewall)))

(defn not-caught?
  "Returns true if you do not get caught while passing through `firewall` after `delay`."
  [firewall delay]
  (not-any? (fn [[depth range]]
              (= 0 (scanner-position range (+ depth delay))))
            firewall))

(defn find-minimum-delay
  [firewall]
  ; sorting shorter periods first allows the not-caught? computation to short-circuit earlier
  (let [firewall (sort-by second firewall)]
    (first (filter (partial not-caught? firewall) (range)))))
