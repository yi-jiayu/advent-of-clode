(ns aoc.year2017.day10)

; rotate function from https://groups.google.com/forum/#!topic/clojure/SjmevTjZPcQ
(defn rotate
  "Take a collection and left rotates it n steps.
  If n is negative, the collection is rotated right. Executes in O(n) time."
  [n coll]
  (let [c (count coll)]
    (take c (drop (mod n c) (cycle coll)))))

(defn reverse-sublist
  "Reverses the `length` element long sublist of `l` starting from the element at index `start`."
  [l start length]
  (if (< length 2)
    ; lengths of 0 or 1 have no effect
    l
    (let [l' (into [] (rotate start l))
          mid (subvec l' 0 length)
          aft (subvec l' length)
          l'' (into [] (concat (reverse mid) aft))]
      (rotate (- start) l''))))
