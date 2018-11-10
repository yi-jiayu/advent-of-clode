(ns aoc.core)

(defn vadd
  [v1 v2]
  (map + v1 v2))

(defn adjacent [xy]
  (map (partial vadd xy) [[1 0] [-1 0] [0 1] [0 -1]]))
