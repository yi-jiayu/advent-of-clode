(ns aoc.year2017.day10)

(def standard-length-suffix-values [17 31 73 47 23])
(def max-length 256)
(def num-rounds 64)

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

(defn knot-hash-round
  "Applies one round of knot-tying and rearranging marks to `l` based on `lengths`."
  ([lengths l curr-pos skip-size]
   (reduce knot-hash-update [l curr-pos skip-size] lengths)))

(defn parse-input [input]
  (map #(Integer/parseInt %) (clojure.string/split input #",")))

(defn parse-input-bytes
  "Parse the input as a byte string, returning a vector of lengths between 0 and 255."
  [input]
  (map int input))

(defn knot-hash-rounds
  "Runs 64 rounds of knot-tying and rearranging marks according to `lengths` and returns the final order."
  [lengths]
  (let [lengths (into [] (concat lengths standard-length-suffix-values))]
    (loop [rounds-remaining num-rounds
           [l curr-pos skip-size] [(into [] (take max-length (range))) 0 0]]
      (if (> rounds-remaining 0)
        (recur (- rounds-remaining 1)
               (knot-hash-round lengths l curr-pos skip-size))
        l))))

(defn condense-hash
  "Condenses the sparse hash obtained after running the knot hash rounds into a shorter dense hash."
  [l]
  (map (partial apply bit-xor) (partition 16 l)))

(defn to-hex
  "Converts the condensed hash into its hexadecimal representation."
  [condensed-hash]
  (apply str (map (partial format "%02x") condensed-hash)))

(defn knot-hash
  "This hash function simulates tying a knot in a circle of string with 256 marks on it
Based on the input to be hashed, the function repeatedly selects a span of string, brings the ends together, and gives
the span a half-twist to reverse the order of the marks within it. After doing this many times, the order of the marks
is used to build the resulting hash."
  [input]
  (-> input
      parse-input-bytes
      knot-hash-rounds
      condense-hash
      to-hex))
