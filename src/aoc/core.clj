(ns aoc.core)

(defn vadd
  [v1 v2]
  (map + v1 v2))

(defn adjacent [xy]
  (map (partial vadd xy) [[1 0] [-1 0] [0 1] [0 -1]]))

; rotate function from https://groups.google.com/forum/#!topic/clojure/SjmevTjZPcQ
(defn rotate
  "Take a collection and left rotates it n steps.
  If n is negative, the collection is rotated right. Executes in O(n) time."
  [n coll]
  (let [c (count coll)]
    (take c (drop (mod n c) (cycle coll)))))
