(ns aoc.year2017.day14
  (:require [aoc.year2017.day10 :refer [knot-hash]])
  (:require [aoc.year2016.day13 :refer [count-set-bits]]))

(defn disk-hashes
  ([key-string] (disk-hashes key-string 128))
  ([key-string side-length]
   (pmap (comp (partial take (/ side-length 8))
               knot-hash
               (partial str key-string "-"))
         (range side-length))))

(defn used-squares
  [hashes]
  (reduce + (map count-set-bits (flatten hashes))))

(defn to-bin
  "Converts a list of integers into its binary representation."
  [ints]
  (apply str (map
              (comp #(format "%8s" %)
                    #(Integer/toBinaryString %))
              ints)))

(defn to-grid
  [disk-hashes]
  (map to-bin disk-hashes))

(defn used?
  ([grid [row col]] (used? grid row col))
  ([grid row col]
   (= \1 (nth (nth grid row) col))))

(defn in-grid?
  [num-rows num-cols [row col]]
  (and (<= 0 row) (<= 0 col) (< row num-rows) (< col num-cols)))

(defn neighbours
  [grid xy]
  (into #{} (filter (every-pred
                     (partial in-grid? (count grid) (count (first grid)))
                     (partial used? grid))
                    (aoc.core/adjacent xy))))

(defn explore
  [grid [row col]]
  (loop [frontier [[row col]]
         explored #{}]
    (let [[row col] (peek frontier)
          frontier (pop frontier)
          explored (conj explored [row col])]
      (let [adjacent (remove explored (neighbours grid [row col]))
            frontier (apply (partial conj frontier) adjacent)]
        (if (empty? frontier)
          explored
          (recur frontier
                 explored))))))

(defn count-regions
  [grid]
  (let [squares (for [row (range (count grid))
                      col (range (count (first grid)))]
                  [row col])]
    (let [[_ num-regions] (reduce (fn [[visited num-regions] square]
                                    (cond
                                      (not (used? grid square)) [visited num-regions]
                                      (visited square) [visited num-regions]
                                      :else [(apply (partial conj visited) (explore grid square))
                                             (+ 1 num-regions)]))
                                  [#{} 0]
                                  squares)]
      num-regions)))
