(ns aoc.core)

(defn vadd
  [v1 v2]
  (mapv + v1 v2))

(defn adjacent [xy]
  "Returns the four adjacent coordinates to coordinates `xy` on a square grid.
  If `xy` is in the format [row col], the returned coordinates are in reading
  order."
  (map (partial vadd xy) [[-1 0] [0 -1] [0 1] [1 0]]))

; rotate function from https://groups.google.com/forum/#!topic/clojure/SjmevTjZPcQ
(defn rotate
  "Take a collection and left rotates it n steps.
  If n is negative, the collection is rotated right. Executes in O(n) time."
  [n coll]
  (let [c (count coll)]
    (take c (drop (mod n c) (cycle coll)))))
