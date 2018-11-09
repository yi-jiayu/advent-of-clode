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

(defn scanner-position [r t]
  (let [positions (into [] (concat (range (- r 1)) (range (- r 1) 0 -1)))]
    (get positions (rem t (* 2 (- r 1))))))

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
  (loop [firewall firewall
         severity 0
         depth 0]
    (if (>= depth (count firewall))
      severity
      (let [firewall (calculate-scanner-positions firewall depth)
            [range scanner-position] (get firewall depth)]
        (let [severity (if (= 0 scanner-position)
                         (+ severity (* depth range))
                         severity)]
          (recur firewall
                 severity
                 (+ 1 depth)))))))
