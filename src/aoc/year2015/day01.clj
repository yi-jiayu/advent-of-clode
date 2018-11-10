(ns aoc.year2015.day01)

(defn final-floor [instructions]
  (reduce (fn [floor instr]
            (case instr
              \( (+ floor 1)
              \) (- floor 1)))
          0
          instructions))

(defn first-step-into-basement [instructions]
  (loop [position 0
         floor 0
         instructions instructions]
    (if (= floor -1) position
        (recur (+ position 1)
               (case (first instructions)
                 \( (+ floor 1)
                 \) (- floor 1))
               (rest instructions)))))
