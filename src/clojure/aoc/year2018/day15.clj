(ns aoc.year2018.day15
  (:require [clojure.string :as string]
            [aoc.core :refer [adjacent]])
  (:import (clojure.lang PersistentQueue)))

(def ^:dynamic *debug* false)

(def starting-hp 200)

(defrecord Battle [cavern units])
(defrecord Unit [type hp])

(defmethod print-method Battle [battle ^java.io.Writer w]
  (.write w (str "(->Battle" (:cavern battle) (:units battle) ")")))

(defmethod print-method Unit [unit ^java.io.Writer w]
  (.write w (str "(->Unit " (:type unit) " " (:hp unit) ")")))

(defn enumerate2d
  "Returns a lazy sequence consisting of [row col elem] for each element in matrix."
  [matrix]
  (apply concat (map-indexed (fn [n row]
                               (map-indexed (fn [m val]
                                              [n m val]) row)) matrix)))

(defn parse-cavern-map
  [input]
  (->> input
       string/trim
       string/split-lines
       (map string/trim)
       (mapv (partial into []))))

(defn extract-units
  [cavern]
  (reduce (fn [units [row col c]]
            (case c
              \E (assoc units [row col] (->Unit \E starting-hp))
              \G (assoc units [row col] (->Unit \G starting-hp))
              units))
          {}
          (enumerate2d cavern)))

(defn parse-input
  [input]
  (let [map (parse-cavern-map input)
        units (extract-units map)]
    (->Battle map units)))

(defn identify-targets
  "Identify all possible targets for unit at `position`. If there are no
  targets remaining, returns nil."
  [{units :units} position]
  (let [unit-type (get-in units [position :type])
        targets (keep (fn [[pos {type :type}]] (when (not= unit-type type) pos)) units)]
    (when (not (empty? targets))
      targets)))

(defn identify-open-squares
  "Identifies all open squares in range of each target in `targets`. If there
  are no open squares, returns nil."
  [{cavern :cavern} targets]
  (let [adjacent-squares (mapcat adjacent targets)
        open-squares (filter (fn [square] (= \. (get-in cavern square))) adjacent-squares)]
    (when (not (empty? open-squares))
      (into #{} open-squares))))

(defn choose-next-step
  "Returns the next step to take to reach the chosen open square in `open-squares`. If
  none of the open squares are reachable, returns nil."
  [{cavern :cavern} position open-squares]
  (let [choices (sort-by first (for [adjacent (adjacent position)
                                     :when (= \. (get-in cavern adjacent))]
                                 [adjacent adjacent]))]
    (loop [open (apply conj PersistentQueue/EMPTY choices)
           closed #{}]
      (when-not (empty? open)
        (let [[choice curr] (peek open)]
          (if (open-squares curr)
            choice
            (let [open (pop open)
                  available-moves (->> curr
                                       adjacent
                                       (remove closed)
                                       (keep (fn [adj] (when (= \. (get-in cavern adj)) [choice adj])))
                                       (sort-by first))]
              (recur (apply conj open available-moves)
                     (conj closed curr)))))))))

(defn enemy-in-range
  "If a unit at `start-pos` is in range of at least one enemy unit represented
  by `target`, returns the position of the enemy unit in range with the lowest
  hit points."
  [battle start-pos target]
  (let [cavern (:cavern battle)
        enemy-positions (sort-by first (keep (fn [[pos tile]] (if (= target tile) pos))
                                             (for [adjacent (adjacent start-pos)]
                                               [adjacent (get-in cavern adjacent)])))]
    (if (not (empty? enemy-positions))
      (let [enemy-units (for [pos enemy-positions] [pos (get-in battle [:units target pos])])
            [pos _] (reduce (fn [[pos min-hp] [unit-pos unit-hp]] (if (< unit-hp min-hp)
                                                                    [unit-pos unit-hp]
                                                                    [pos min-hp]))
                            enemy-units)]
        pos))))

(defn find-target
  "Returns the next tile the unit should move to."
  [cavern start-pos target]
  (let [choices (sort-by first (for [adjacent (adjacent start-pos)
                                     :when (= \. (get-in cavern adjacent))]
                                 [adjacent adjacent]))]
    (loop [open (apply conj PersistentQueue/EMPTY choices)
           closed #{}]
      (let [[choice curr] (peek open)
            open (pop open)
            adjacent (sort-by first (for [adjacent (adjacent curr)
                                          :when (not (closed adjacent))]
                                      [adjacent (get-in cavern adjacent)]))]
        (if (empty? open)
          nil
          (if (first (filter (fn [[_ tile]] (= target tile)) adjacent))
            [choice curr]
            (let [available-moves (keep (fn [[pos tile]] (if (= \. tile) [choice pos])) adjacent)]
              (recur (apply conj open available-moves)
                     (conj closed curr)))))))))

(defn move-unit
  "Moves the unit at pos to new pos."
  [{:keys [cavern units]} pos new-pos]
  (let [unit (get-in cavern pos)
        faction (units unit)
        hp (faction pos)]
    (->Battle (-> cavern
                  (assoc-in pos \.)
                  (assoc-in new-pos unit))
              (assoc units unit (-> faction
                                    (dissoc pos)
                                    (assoc new-pos hp))))))

(defn attack
  "`attacker` deals damage equal to `attack-power` to `defender`. `defender` is
  removed from the battle if its remaining hit points are 0 or fewer."
  [{:keys [cavern units]} attacker attack-power defender]
  (let [defender-unit-type (get-in cavern defender)
        defender-faction (units defender-unit-type)
        defender-hp-before (defender-faction defender)
        defender-hp-after (- defender-hp-before attack-power)
        [cavern defender-faction-after] (if (>= 0 defender-hp-after)
                                          [(assoc-in cavern defender \.) (dissoc defender-faction defender)]
                                          [cavern (assoc defender-faction defender defender-hp-after)])]
    (->Battle cavern (assoc units defender-unit-type defender-faction-after))))

(defn move-if-enemy-not-in-range
  [{:keys [cavern _] :as battle} pos target]
  (if (not (enemy-in-range battle pos target))
    (if-let [[destination _] (find-target cavern pos target)]
      [(move-unit battle pos destination) destination]
      [battle pos])
    [battle pos]))

(defn attack-if-enemy-in-range
  [[battle pos] target]
  (if-let [target-pos (enemy-in-range battle pos target)]
    (attack battle pos 3 target-pos)
    battle))

(defn take-turn
  "The unit at `pos` takes a turn."
  [{:keys [cavern _] :as battle} pos]
  (let [faction (get-in cavern pos)
        target (case faction \E \G \G \E)]
    (-> battle
        (move-if-enemy-not-in-range pos target)
        (attack-if-enemy-in-range target))))

;(defn simulate-battle
;  "Simulates a battle from start to finish and returns the outcome."
;  [{:keys [cavern units] :as battle}]
;  (loop [all-units (sort (mapcat keys (vals units)))]
;    (reduce (fn [battle unit]))))