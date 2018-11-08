(ns aoc.year2016.day13)

(defn calculate-z-value [x y]
  (+ (* x x) (* 3 x) (* 2 x y) y (* y y)))

(defn count-set-bits [n]
  (loop [set 0
         n n]
    (if (zero? n)
      set
      (recur (+ set (bit-and n 1))
             (bit-shift-right n 1)))))

(defn open? [fav [x y]]
  (even? (count-set-bits (+ (calculate-z-value x y) fav))))

(defn add-pair [[x y] [dx dy]]
  (vector (+ x dx) (+ y dy)))

(defn adjacent [xy]
  (map #(add-pair xy %) [[1 0] [-1 0] [0 1] [0 -1]]))

(defn first-quadrant? [[x y]]
  (and (not (neg? x)) (not (neg? y))))

(defn neighbours [fav xy]
  (filter (every-pred (partial open? fav) first-quadrant?)
          (adjacent xy)))

(defn steps-to [fav dest]
  (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY [[1 1] 0])
         explored #{}]
    (let [[curr depth] (peek frontier) frontier (pop frontier)]
      (if (= dest curr)
        depth
        (let [frontier (apply (partial conj frontier)
                              (map #(vector % (+ depth 1))
                                   (remove explored (neighbours fav curr))))]
          (recur frontier
                 (conj explored curr)))))))
