(ns aoc.year2018.day14)

(defn combine-recipes
  "Returns the new recipes created from the two current recipes"
  [first second]
  (let [sum (+ first second)]
    (if (> 10 sum)
      [sum]
      [(quot sum 10) (rem sum 10)])))

(defn scoreboard-after-n-recipes
  [n]
  (loop [scoreboard [3 7]
         elf1 0
         elf2 1]
    (if (<= n (count scoreboard))
      scoreboard
      (let [new-recipes (combine-recipes (scoreboard elf1) (scoreboard elf2))
            scoreboard (apply conj scoreboard new-recipes)
            elf1 (rem (+ 1 elf1 (scoreboard elf1)) (count scoreboard))
            elf2 (rem (+ 1 elf2 (scoreboard elf2)) (count scoreboard))]
        (recur scoreboard
               elf1
               elf2)))))

(defn last-m-recipes-after-making-n
  [n m]
  (take m (drop n (scoreboard-after-n-recipes (+ n m)))))

(defn scoreboard-until-specfic-recipe-sequence
  [specific-recipes]
  (let [specific-recipes-length (count specific-recipes)]
    (loop [scoreboard [3 7 1 0 1 0 1]
           elf1 6
           elf2 4]
      (let [num-recipes (count scoreboard)]
        (cond
          (= specific-recipes (subvec scoreboard (- num-recipes specific-recipes-length))) (- num-recipes specific-recipes-length)
          (= specific-recipes (subvec scoreboard (- num-recipes specific-recipes-length 1) (- num-recipes 1))) (- num-recipes specific-recipes-length 1)
          :else (let [new-recipes (combine-recipes (scoreboard elf1) (scoreboard elf2))
                      scoreboard (apply conj scoreboard new-recipes)
                      elf1 (rem (+ 1 elf1 (scoreboard elf1)) (count scoreboard))
                      elf2 (rem (+ 1 elf2 (scoreboard elf2)) (count scoreboard))]
                  (recur scoreboard
                         elf1
                         elf2)))))))
