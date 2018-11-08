(ns aoc.year2015.day01)

(def left-parens \()
(def right-parens \))

(defn final-floor [instructions]
  (reduce #(condp = %2
             left-parens (+ %1 1)
             right-parens (- %1 1))
          0
          instructions))

(defn first-step-into-basement [instructions]
  (loop [position 0
         floor 0
         instructions instructions]
    (if (= floor -1) position
        (recur (+ position 1)
               (condp = (first instructions)
                 left-parens (+ floor 1)
                 right-parens (- floor 1))
               (rest instructions)))))
