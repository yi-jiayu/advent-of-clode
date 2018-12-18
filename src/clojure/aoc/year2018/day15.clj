(ns aoc.year2018.day15
  (:require [clojure.string :as string]
            [aoc.core :refer [adjacent]])
  (:import (clojure.lang PersistentQueue)))

(def ^:dynamic *debug* false)

(def starting-hp 200)
(def attack-power 3)

(defrecord Battle [cavern units])
(defrecord Unit [type hp])

(defmethod print-method Battle [battle ^java.io.Writer w]
  (.write w (str "(->Battle" (clojure.string/join (doall (map prn-str (:cavern battle)))) (:units battle) ")")))

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
          (sorted-map)
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

(defn find-target-in-range
  "If a unit at `pos` is in range of at least one target in targets, returns
  the target's position, otherwise nil. When there is more than one target in
  range, the one with the lowest hit points remaining is chosen."
  [{units :units} pos]
  (let [unit-type (get-in units [pos :type])
        adjacent (->> pos
                      adjacent
                      (keep (fn [pos] (when-let [{:keys [type hp]} (units pos)]
                                        (when (not= unit-type type) [pos hp])))))]
    (when (not (empty? adjacent))
      (first (reduce (fn [[pos min-hp] [pos' hp]] (if (< hp min-hp)
                                                    [pos' hp]
                                                    [pos min-hp]))
                     adjacent)))))

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
  (let [choices (for [adjacent (adjacent position)
                      :when (= \. (get-in cavern adjacent))]
                  [adjacent adjacent])]
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
                                       (keep (fn [adj] (when (= \. (get-in cavern adj)) [choice adj]))))]
              (recur (apply conj open available-moves)
                     (conj closed curr)))))))))

(defn move-unit
  "Moves the unit at pos to new pos."
  [{:keys [cavern units]} pos new-pos]
  (let [unit-type (get-in cavern pos)
        unit (units pos)]
    (->Battle (-> cavern
                  (assoc-in pos \.)
                  (assoc-in new-pos unit-type))
              (-> (do units)
                  (dissoc pos)
                  (assoc new-pos unit)))))

(defn attack
  [{:keys [cavern units]} target]
  (let [{target-hp :hp} (units target)
        remaining-hp (- target-hp attack-power)]
    (if (<= remaining-hp 0)
      (->Battle (assoc-in cavern target \.) (dissoc units target))
      (->Battle cavern (update units target #(assoc % :hp remaining-hp))))))

(defn move-if-enemy-not-in-range
  [battle pos]
  (if (nil? (find-target-in-range battle pos))
    (if-let [new-pos (some->> pos
                              (identify-targets battle)
                              (identify-open-squares battle)
                              (choose-next-step battle pos))]
      [(move-unit battle pos new-pos) new-pos]
      [battle pos])
    [battle pos]))

(defn attack-if-enemy-in-range
  [[battle pos]]
  (if-let [target (find-target-in-range battle pos)]
    (attack battle target)
    battle))

(defn take-turn
  "The unit at `pos` takes a turn. Returns nil if the battle is over."
  [battle pos]
  (when (not (empty? (identify-targets battle pos)))
    (some-> battle
            (move-if-enemy-not-in-range pos)
            (attack-if-enemy-in-range))))

(defn take-all-turns
  "The units in the battle all take their turns in sequence. If any unit is no
  longer alive when its turn comes, its turn is skipped. Returns the resulting
  state of the battle and a boolean representing whether the battle is over."
  [{units :units :as battle}]
  (let [turn-order (keys units)]
    (reduce (fn [[{units :units :as battle} _] unit]
              (if (contains? units unit)
                (if-let [after (take-turn battle unit)]
                  [after false]
                  [battle true])
                [battle false]))
            [battle false]
            turn-order)))

(defn calculate-outcome
  "Calculates the outcome of the battle."
  [{units :units} rounds]
  (let [sum-hit-points (apply + (map :hp (vals units)))]
    (* sum-hit-points rounds)))

(defn simulate-battle
  "Simulates a battle from start to finish and returns the outcome."
  [battle]
  (loop [battle battle
         round 0]
    (let [[battle finished?] (take-all-turns battle)]
      (if finished?
        (calculate-outcome battle round)
        (recur battle
               (inc round))))))