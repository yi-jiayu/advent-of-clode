(ns aoc.year2018.day11)

(defn get-hundreds-digit
  "Returns the 100s digit of `x`"
  [x]
  (if (< x 100)
    0
    (-> x
        (quot 100)
        (rem 10))))

(defn calculate-power-level
  "Calculates the power level of the fuel cell at coordinate `X`, `Y` in a grid with the given serial number."
  [grid-serial-number x y]
  (let [rack-id (+ x 10)
        power-level (* y rack-id)]
    (-> power-level
        (+ grid-serial-number)
        (* rack-id)
        (get-hundreds-digit)
        (- 5))))

(defn partition-multiple
  "Partitions multiple collections and concatenates the corresponding partitions."
  [n step colls]
  (->> colls
       (map (partial partition n step))
       (apply map concat)))

(defn partition2d
  "Partitions a 2d array into subarrays."
  [n step coll]
  (mapcat (partial partition-multiple n step)
          (partition n step coll)))

(defn convolve
  "Convolves `f` over `coll` given `n` and `step`."
  [f n step coll]
  (map f (partition2d n step coll)))

(defn index-to-coord
  "Converts the index of a convolution output from a rows x cols matrix with size n and step to the top-left coordinate of the input."
  [_ cols n step index]
  (let [row (quot index (- cols n -1))
        col (rem index (- cols n -1))]
    [(* step row) (* step col)]))
