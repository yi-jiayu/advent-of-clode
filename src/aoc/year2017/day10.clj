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

(defn knot-hash-update
  "Updates `l` based on `curr-pos`, `skip-size` and `length`. Returns a tuple of the new l, curr-pos and skip-size."
  [[l curr-pos skip-size] length]
  (let [l' (reverse-sublist l curr-pos length)
        curr-pos' (rem (+ curr-pos length skip-size) (count l))
        skip-size' (+ skip-size 1)]
    [l' curr-pos' skip-size']))

(defn knot-hash
  "This hash function simulates tying a knot in a circle of string with 256 marks on it
Based on the input to be hashed, the function repeatedly selects a span of string, brings the ends together, and gives
the span a half-twist to reverse the order of the marks within it. After doing this many times, the order of the marks
is used to build the resulting hash."
  ([lengths] (knot-hash 256 lengths))
  ([max-length lengths]
   (let [l (into [] (range max-length))
         l' (first (reduce knot-hash-update [l 0 0] lengths))]
     (apply * (take 2 l')))))

(defn parse-input [input]
  (map #(Integer/parseInt %) (clojure.string/split input #",")))
