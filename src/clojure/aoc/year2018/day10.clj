(ns aoc.year2018.day10
  (:require [aoc.core :refer [vadd]]))

(defn parse-particle-position-and-velocity
  "Returns the initial position and velocity for a particle."
  [input]
  (->> input
       (re-matches #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")
       (rest)
       (map #(Integer/parseInt %))))

(defn load-particles
  "Initialises the location and velocity of a list of particles."
  [input]
  (->> input
       (clojure.string/trim)
       (clojure.string/split-lines)
       (map clojure.string/trim)
       (map parse-particle-position-and-velocity)))

(defn update-particle-position
  "Updates a particle's position based on its velocity."
  [particle]
  (let [[px py vx vy] particle
        [px' py'] (vadd [px py] [vx vy])]
    [px' py' vx vy]))

(defn update-particle-positions
  "Updates the positions of all particles."
  [particles]
  (map update-particle-position particles))

(defn get-bounding-box
  "Returns the maximum and minimum x and y values out of all the particles."
  [particles]
  (let [xs (map first particles)
        max-x (apply max xs)
        min-x (apply min xs)
        ys (map second particles)
        max-y (apply max ys)
        min-y (apply min ys)]
    [max-x min-x max-y min-y]))

(defn bounding-box-size
  "Returns the sum of the width and height of the bounding box containing particles."
  [particles]
  (let [[x1 x2 y1 y2] (get-bounding-box particles)]
    (+ (Math/abs ^int (- x1 x2)) (Math/abs ^int (- y1 y2)))))

(defn plot-particle
  "Plots a particle on background."
  [background x-offset y-offset particle]
  (let [[x' y' _ _] particle
        x (- x' x-offset)
        y (- y' y-offset)]
    (assoc-in background [y x] 1)))

(defn plot-particles
  "Plots particles based on their positions on a grid."
  [particles]
  (let [[x-max x-offset y-max y-offset] (get-bounding-box particles)
        width (inc (- x-max x-offset))
        height (inc (- y-max y-offset))
        background (into [] (repeat height (into [] (repeat width 0))))]
    (reduce #(plot-particle %1 x-offset y-offset %2)
            background
            particles)))

(defn render-particle-plot
  "Renders a plot of particles to a string."
  [plot]
  (clojure.string/join "\n"
                       (mapv #(apply str (map {0 " " 1 "#"} %)) plot)))

(defn fast-forward
  "Fast-forward the movements of particles, stopping when the bounding box size is at a minimum, returning the time and particles when that happens."
  [particles]
  (loop [t 0
         particles particles
         score (bounding-box-size particles)]
    (let [particles' (update-particle-positions particles)
          score' (bounding-box-size particles')]
      (if (< score score')
        [t particles]
        (recur (inc t)
               particles'
               score')))))
