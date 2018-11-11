(ns aoc.year2017.day16)

(defn spin
  "Makes `x` programs move from the end to the front, but maintain their order otherwise."
  [x programs]
  (into [] (aoc.core/rotate (- x) programs)))

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

(defn parse-input
  [input]
  (map #(case (first %)
          \s [:s (Integer/parseInt (subs % 1))]
          \x (let [[a b] (clojure.string/split (subs % 1) #"/")]
               [:x (Integer/parseInt a) (Integer/parseInt b)])
          \p [:p (nth % 1) (nth % 3)])
       (clojure.string/split input #",")))

(defn programs
  [n]
  (mapv char (range (int \a) (+ (int \a) n))))

(defn dance
  [moves programs]
  (reduce (fn [programs move]
            (case (first move)
              :s (spin (second move) programs)
              :x (exchange (second move) (nth move 2) programs)
              :p (partner (second move) (nth move 2) programs)))
          programs
          moves))

