(ns aoc.year2018.day13
  (:require [clojure.string :refer [trimr split-lines] :as strings]
            [aoc.core :refer [vadd]]))

(def directions {:up    [0 -1]
                 :down  [0 1]
                 :left  [-1 0]
                 :right [1 0]})

(def turn-left {:up    :left
                :down  :right
                :left  :down
                :right :up})

(def turn-right {:left  :up
                 :right :down
                 :down  :left
                 :up    :right})

(def straight identity)

(def next-turn [turn-left straight turn-right])

(defrecord Cart [position direction state])

(defn remove-carts-from-track
  "Replaces carts with a pieces of track in the appropriate direction."
  [track]
  (map #(-> %
            (strings/replace #"\^|v" "|")
            (strings/replace #"<|>" "-"))
       track))

(defn make-cart
  "Makes a cart given its initial position and character representation, returning nil if character does not represent a cart."
  [y x c]
  (when-let [direction ({\^ :up
                         \v :down
                         \< :left
                         \> :right} c)]
    (->Cart [x y] direction 0)))

(defn carts-in-row
  "Returns all the carts represented in row `n`"
  [n row]
  (keep-indexed (partial make-cart n) row))

(defn parse-input
  "Returns a map and a list of carts."
  [input]
  (let [track (-> input (trimr) (split-lines))
        carts (flatten (map-indexed carts-in-row track))]
    [(remove-carts-from-track track) carts]))

(defn move-cart
  "Moves cart one step along track."
  [track {:keys [direction position state]}]
  (let [[x' y'] (vadd position (directions direction))
        track-piece (get-in track [y' x'])
        [direction' state'] (cond
                              (and (#{:left :right} direction) (= track-piece \/)) [(turn-left direction) state]
                              (and (#{:left :right} direction) (= track-piece \\)) [(turn-right direction) state]
                              (and (#{:up :down} direction) (= track-piece \/)) [(turn-right direction) state]
                              (and (#{:up :down} direction) (= track-piece \\)) [(turn-left direction) state]
                              (= track-piece \+) [((next-turn state) direction) (rem (inc state) 3)]
                              :else [direction state])]
    (->Cart [x' y'] direction' state')))

(defn sort-carts
  "Sorts carts in the order they will move each tick."
  [carts]
  (sort-by (juxt (comp second :position) (comp first :position)) carts))

(defn tick
  "Moves each cart in order and returns their new positions and the location of the first collision, if any."
  [track carts]
  (let [sorted-carts (sort-carts carts)
        [carts' collisions _] (reduce (fn [[carts' collisions positions] cart]
                                        (let [cart' (move-cart track cart)
                                              position (:position cart')
                                              collisions (if (positions position)
                                                           (conj collisions position)
                                                           collisions)
                                              positions (conj positions position)
                                              carts' (conj carts' cart')]
                                          [carts' collisions positions]))
                                      [[] [] #{}]
                                      sorted-carts)]
    [carts' (first collisions)]))

(defn render-carts-on-track
  "Returns a string displaying carts on track."
  [track carts]
  (let [track' (into [] (map (partial into []) track))]
    (->> (reduce (fn [track cart]
                   (assoc-in track
                             (reverse (:position cart))
                             (case (:direction cart)
                               :up \^
                               :down \v
                               :left \<
                               :right \>)))
                 track'
                 carts)
         (map (partial apply str))
         (strings/join "\n"))))

(defn run-until-collision
  "Runs carts on track until a collision happens, then returns that collision."
  [track carts]
  (loop [carts carts
         collision nil]
    (if collision
      collision
      (let [[carts collision] (tick track carts)]
        (prn track carts)
        (println (render-carts-on-track track carts))
        (recur carts
               collision)))))
