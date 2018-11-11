(ns aoc.year2017.day15)

(defn next-value
  "Calculates the next value for a generator given its factor, modulus and the previous value."
  [factor modulus prev]
  (rem (* factor prev) modulus))

(defn generator
  "Returns a lazy sequence representing the values produced by a generator with the given initial value, factor and modulus."
  [initial factor modulus]
  (let [next (partial next-value factor modulus)]
    (fn gen
      ([] (gen initial))
      ([prev] (lazy-seq (cons (next prev) (gen (next prev))))))))
