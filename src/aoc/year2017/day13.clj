(ns aoc.year2017.day13
  (:require [clojure.string :refer [split split-lines]]))

(defn initialise-firewall
  "Creates a firewall from its text representation."
  [input]
  (let [lines (map #(split % #": ") (split-lines input))
        layers (map (fn [[depth range]] [(Integer/parseInt depth) (Integer/parseInt range)]) lines)
        num-layers (first (last layers))]
    (reduce (fn [firewall [depth range]] (assoc firewall depth [range 0]))
            (apply vector (repeat num-layers [0 0]))
            layers)))

(defn scanner-position
  "Calculate the scanner position in a layer with range `r` at time `t`.
  The scanner starts at position 0 at time 0."
  [r t]
  (let [r' (- r 1)
        positions (into [] (concat (range r') (range r' 0 -1)))]
    (get positions (rem t (* 2 r')))))

(defn calculate-scanner-positions
  "Calculates scanner positions within `firewall` at time `t`."
  [firewall t]
  (mapv (fn [[range position]]
          (if (zero? range)
            [range position]
            [range (scanner-position range t)]))
        firewall))

(defn calculate-severity
  "Calculates the total severity of a trip through a firewall."
  [firewall]
  (loop [severity 0
         depth 0]
    (if (>= depth (count firewall))
      severity
      (let [firewall (calculate-scanner-positions firewall depth)
            [range scanner-position] (get firewall depth)
            severity (if (= 0 scanner-position)
                       (+ severity (* depth range))
                       severity)]
        (recur severity
               (+ 1 depth))))))

(defn caught?
  "Returns true if you get caught passing through `firewall` after `delay`."
  [firewall delay]
  (loop [depth 0
         time delay]
    (if (>= depth (count firewall))
      false
      (let [firewall (calculate-scanner-positions firewall time)
            [range scanner-position] (get firewall depth)]
        (if (and (not (= 0 range)) (= 0 scanner-position))
          true
          (recur (+ 1 depth)
                 (+ 1 time)))))))

(defn find-minimum-delay
  [firewall]
  (loop [delay 0]
    (if (caught? firewall delay)
      (recur (+ 1 delay))
      delay)))
