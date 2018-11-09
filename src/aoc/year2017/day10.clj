(ns aoc.year2017.day10)

(defn reverse-sublist
  "Reverses the `length` element long sublist of `l` starting from the element at index `start`."
  [l start length]
  (let [bef (subvec l 0 start)
        mid (subvec l start (+ start length))
        aft (subvec l (+ start length))]
    (into [] (concat bef (reverse mid) aft))))