(ns aoc.year2017.day15)

(defn next-value
  "Calculates the next value for a generator given its factor, modulus and the previous value."
  [factor modulus prev]
  (rem (* factor prev) modulus))

(defn generator
  "Returns a lazy sequence representing the values produced by a generator with the given initial value, factor and modulus."
  ([initial factor modulus] (generator nil factor modulus initial))
  ([_ factor modulus prev]
   (let [next (partial next-value factor modulus)]
     (lazy-seq (cons (next prev) (generator _ factor modulus (next prev)))))))

(defn generator-a
  [initial]
  (generator initial 16807 2147483647))

(defn generator-b
  [initial]
  (generator initial 48271 2147483647))

(defn lowest-bits-match?
  "Returns true if the lowest 16 bits of `a` and `b` match."
  [a b]
  (= (bit-and a 0xffff) (bit-and b 0xffff)))

(defn judge
  "Returns a lazy sequence representing whether the last 16 bits of each value produced by `a` and `b` match."
  [a b]
  (map lowest-bits-match? a b))

(defn final-count
  "Counts the number of values out of `n` produced by `a` and `b` with matching lowest 16 bits."
  [a b n]
  (->> (judge a b)
       (take n)
       (filter identity)
       (count)))

(defn parse-input
  [input]
  (map (comp #(Integer/parseInt % 10)
             last
             #(clojure.string/split %1 #" "))
       (clojure.string/split-lines input)))

(defn multiples-of-4
  [generator]
  (filter #(zero? (bit-and % 3)) generator))

(defn multiples-of-8
  [generator]
  (filter #(zero? (bit-and % 7)) generator))

(defn generator-a'
  [initial]
  (multiples-of-4 (generator-a initial)))

(defn generator-b'
  [initial]
  (multiples-of-8 (generator-b initial)))
