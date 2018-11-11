(ns aoc.year2017.day16)

(defn spin
  "Makes `x` programs move from the end to the front, but maintain their order otherwise."
  [x programs]
  (aoc.core/rotate (- x) programs))

(defn exchange
  "Makes the programs at positions `a` and `b` swap places."
  [a b programs]
  (let [pa (nth programs a)
        pb (nth programs b)]
    (-> programs
        (assoc a pb)
        (assoc b pa))))

(defn partner
  "Makes the programs named `a` and `b` swap places."
  [a b programs]
  (let [ia (.indexOf programs a)
        ib (.indexOf programs b)]
    (exchange ia ib programs)))
