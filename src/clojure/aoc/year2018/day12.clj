(ns aoc.year2018.day12)

(defn parse-rule
  "Parses a single rule for whether a pot will contain a plant in the next generation."
  [rule]
  [(take 5 rule) (last rule)])

(defn parse-rules
  "Parses the rules for whether a pot will contain a plant in the next generation."
  [rules]
  (->> rules
       (clojure.string/trim)
       (clojure.string/split-lines)
       (map clojure.string/trim)
       (mapcat parse-rule)
       (apply hash-map)))

(defn extract-initial-state
  "Extracts the initial state from the first line of input."
  [input]
  (clojure.string/replace input "initial state: " ""))

(defn parse-input
  "Returns the initial state of the pots and the rules."
  [input]
  (let [[initial-state rules] (clojure.string/split input #"\n\n")]
    [(extract-initial-state initial-state)
     (parse-rules rules)]))

(defn trim-edges
  "Removes pots without plants on either side and updates the start value."
  [pots start]
  (let [empty-pots-left (count (take-while (partial = \.) pots))
        empty-pots-right (count (take-while (partial = \.) (reverse pots)))]
    [(->> pots
          (drop empty-pots-left)
          (drop-last empty-pots-right))
     (+ empty-pots-left start)]))

(defn next-generation
  "Simulates the next generation of pots given the current generation."
  [rules pots start]
  (let [pots' (concat "...." pots "....")]
    (trim-edges (map #(rules % \.) (partition 5 1 pots'))
                (- start 2))))

(defn simulate
  "Simulate the propogation of plants for `n` generations."
  [n rules pots start]
  (nth
    (iterate (partial apply next-generation rules) [pots start])
    n))

(defn sum-pot-numbers-with-plants
  "Sums up all pot numbers containing plants."
  [pots start]
  (apply + (keep-indexed (fn [index item] (if (= \# item) (+ start index))) pots)))
