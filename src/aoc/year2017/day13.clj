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

(defn calculate-severity
  "Calculates the total severity of a trip through a firewall."
  [firewall]
  (apply + (map (fn [[depth range]]
                  (if (= 0 (scanner-position range depth))
                    (* range depth)
                    0))
                firewall)))

(defn caught?
  "Returns true if you get caught passing through `firewall` after `delay`."
  [firewall delay]
  (not (not-any? (fn [[depth range]]
                   (= 0 (scanner-position range (+ depth delay))))
                 firewall)))

(defn find-minimum-delay
  [firewall]
  (loop [delay 0]
    (if (caught? firewall delay)
      (recur (+ 1 delay))
      delay)))
