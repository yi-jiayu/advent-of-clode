(ns aoc.year2018.day13
  (:require [clojure.string :refer [trimr split-lines] :as strings]
            [aoc.core :refer [vadd]]))

(def ^:dynamic *debug* false)

(def directions {:up    [-1 0]
                 :down  [1 0]
                 :left  [0 -1]
                 :right [0 1]})

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

(defmethod print-method Cart [cart ^java.io.Writer w]
  (.write w (str "Cart" (vals (into {} cart)))))

(defn remove-carts-from-track
  "Replaces carts with a pieces of track in the appropriate direction."
  [track]
  (mapv #(-> %
             (strings/replace #"\^|v" "|")
             (strings/replace #"<|>" "-"))
        track))

(defn make-cart
  "Makes a cart given its initial position and character representation, returning nil if character does not represent a cart."
  [row col c]
  (when-let [direction ({\^ :up
                         \v :down
                         \< :left
                         \> :right} c)]
    (->Cart [row col] direction 0)))

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
  (let [position' (vadd position (directions direction))
        track-piece (get-in track position')
        [direction' state'] (cond
                              (and (#{:left :right} direction) (= track-piece \/)) [(turn-left direction) state]
                              (and (#{:left :right} direction) (= track-piece \\)) [(turn-right direction) state]
                              (and (#{:up :down} direction) (= track-piece \/)) [(turn-right direction) state]
                              (and (#{:up :down} direction) (= track-piece \\)) [(turn-left direction) state]
                              (= track-piece \+) [((next-turn state) direction) (rem (inc state) (count next-turn))]
                              :else [direction state])]
    (->Cart position' direction' state')))

(defn sort-carts
  "Sorts carts in the order they will move each tick."
  [carts]
  (sort-by :position carts))

; todo: account for carts colliding with another cart which hasn't moved.
; this wasn't an issue with part 1 but does affect the correctness of the answer.
(defn tick
  "Moves each cart in order and returns their new positions and the location of the first collision, if any."
  [track carts]
  (let [sorted-carts (sort-carts carts)
        [carts' collision] (reduce (fn [[carts' _] cart]
                                     (let [cart' (move-cart track cart)
                                           position (:position cart')]
                                       (if (carts' position)
                                         (reduced [carts' position])
                                         [(assoc carts' position cart') nil])))
                                   [{} nil]
                                   sorted-carts)]
    [(when (not collision) (vals carts')) collision]))

(defn render-carts-on-track
  "Returns a string displaying carts on track."
  [track carts]
  (let [track' (into [] (map (partial into []) track))]
    (->> (reduce (fn [track cart]
                   (assoc-in track
                             (:position cart)
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
        (recur carts
               collision)))))

(defn tick'
  "Moves each cart in order and returns their new positions. If any two carts
  collide, they are both removed from the result."
  [track carts]
  (let [positions-carts (into {} (for [cart carts] [(:position cart) cart]))
        sorted-carts (sort-carts carts)
        carts' (reduce (fn [position-carts cart]
                         (if (contains? position-carts (:position cart))
                           (let [positions-carts (dissoc position-carts (:position cart))
                                 cart' (move-cart track cart)
                                 position' (:position cart')]
                             (if (contains? positions-carts position')
                               (dissoc positions-carts position')
                               (assoc positions-carts position' cart')))
                           position-carts))
                       positions-carts
                       sorted-carts)]
    (into #{} (vals carts'))))

(defn run-until-one-cart-left
  "Runs carts on track, removing carts that collide, until only one cart remains."
  [track carts]
  (loop [carts carts]
    (if (>= 1 (count carts))
      (:position (first carts))
      (recur (tick' track carts)))))
