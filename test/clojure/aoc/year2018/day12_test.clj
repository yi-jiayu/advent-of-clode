(ns aoc.year2018.day12-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day12 :refer :all]))

(def example-input "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
")
(def input (slurp (clojure.java.io/resource "year2018/day12.txt")))

(deftest parse-rule-test
  (is (= [[\. \. \# \. \.] \.] (parse-rule "..#.. => .")))
  (is (= [[\# \# \. \# \#] \.] (parse-rule "##.## => .")))
  (is (= [[\. \# \# \. \#] \#] (parse-rule ".##.# => #"))))

(deftest parse-rules-test
  (is (= {[\# \# \. \# \#] \.
          [\# \# \. \. \#] \#
          [\# \. \. \. \#] \#
          [\. \# \. \# \#] \#
          [\. \. \. \. \#] \.}
         (parse-rules "#...# => #
  ....# => .
  ##..# => #
  .#.## => #
  ##.## => ."))))

(deftest extract-initial-state-test
  (is (= "#..#.#..##......###...###" (extract-initial-state "initial state: #..#.#..##......###...###"))))

(deftest parse-input-test
  (is (= ["#..#.#..##......###...###" {[\. \# \# \# \#] \#,
                                       [\# \. \# \# \#] \#,
                                       [\. \. \. \# \#] \#,
                                       [\# \. \# \. \#] \#,
                                       [\. \# \. \. \.] \#,
                                       [\. \# \# \. \.] \#,
                                       [\. \# \. \# \.] \#,
                                       [\# \# \# \# \.] \#,
                                       [\. \# \. \# \#] \#,
                                       [\# \# \. \# \#] \#,
                                       [\# \# \. \# \.] \#,
                                       [\# \# \# \. \#] \#,
                                       [\# \# \# \. \.] \#,
                                       [\. \. \# \. \.] \#}] (parse-input example-input))))

(deftest trim-edges-test
  (is (= [[\# \. \. \. \# \. \. \. \. \# \. \. \. \. \. \# \. \. \# \. \. \# \. \. \#] 0]
         (trim-edges "..#...#....#.....#..#..#..#.." -2))))

(deftest next-generation-test
  (let [rules (parse-rules example-input)]
    (is (= [[\# \. \. \. \# \. \. \. \. \# \. \. \. \. \. \# \. \. \# \. \. \# \. \. \#] 0] (next-generation rules ["#..#.#..##......###...###" 0])))
    (is (= [[\# \# \. \. \# \# \. \. \. \# \# \. \. \. \. \# \. \. \# \. \. \# \. \. \# \#] 0] (next-generation rules ["#...#....#.....#..#..#..#" 0])))
    (is (= [[\# \. \# \. \. \. \# \. \. \# \. \# \. \. \. \. \# \. \. \# \. \. \# \. \. \. \#] -1] (next-generation rules ["##..##...##....#..#..#..##" 0])))))

(deftest simulate-test
  (let [[initial-state rules] (parse-input example-input)]
    (is (= [[\# \. \. \. \. \# \# \. \. \. \. \# \# \# \# \# \. \. \. \# \# \# \# \# \# \# \. \. \. \. \# \. \# \. \. \# \#] -2]
           (simulate 20 rules initial-state 0)))))

(deftest sum-pot-numbers-with-plants-test
  (is (= 325 (sum-pot-numbers-with-plants "#....##....#####...#######....#.#..##" -2)))
  (let [[initial-state rules] (parse-input input)]
    (is (= 3120 (apply sum-pot-numbers-with-plants (simulate 20 rules initial-state 0))))))

(deftest align-pots-test
  (is (= ["..#..#.#..##......###...###"
          "..#...#....#.....#..#..#..#"
          "..##..##...##....#..#..#..##"
          ".#.#...#..#.#....#..#..#...#"
          "..#.#..#...#.#...#..#..##..##"
          "...#...##...#.#..#..#...#...#"
          "...##.#.#....#...#..##..##..##"
          "..#..###.#...##..#...#...#...#"
          "..#....##.#.#.#..##..##..##..##"
          "..##..#..#####....#...#...#...#"
          ".#.#..#...#.##....##..##..##..##"
          "..#...##...#.#...#.#...#...#...#"
          "..##.#.#....#.#...#.#..##..##..##"
          ".#..###.#....#.#...#....#...#...#"
          ".#....##.#....#.#..##...##..##..##"
          ".##..#..#.#....#....#..#.#...#...#"
          "#.#..#...#.#...##...#...#.#..##..##"
          ".#...##...#.#.#.#...##...#....#...#"
          ".##.#.#....#####.#.#.#...##...##..##"
          "#..###.#..#.#.#######.#.#.#..#.#...#"]
         (let [[initial-state rules] (parse-input example-input)]
           (align-pots (->> [initial-state 0]
                            (iterate (partial next-generation rules))
                            (take 20)))))))
