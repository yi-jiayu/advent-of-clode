(ns aoc.year2018.day15-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day15 :refer :all]))

(deftest enumerate2d-test
  (let [matrix [[1 2 3]
                [4 5 6]
                [7 8 9]]]
    (is (= [[0 0 1]
            [0 1 2]
            [0 2 3]
            [1 0 4]
            [1 1 5]
            [1 2 6]
            [2 0 7]
            [2 1 8]
            [2 2 9]] (enumerate2d matrix)))))

(deftest parse-cavern-map-test
  (is (= ["#######"
          "#.G.E.#"
          "#E.G.E#"
          "#.G.E.#"
          "#######"]
         (parse-cavern-map "#######
                            #.G.E.#
                            #E.G.E#
                            #.G.E.#
                            #######"))))

(deftest extract-units-test
  (is (= {\E (zipmap [[1 4] [2 1] [2 5] [3 4]] (repeat starting-hp))
          \G (zipmap [[1 2] [2 3] [3 2]] (repeat starting-hp))}
         (extract-units ["#######"
                         "#.G.E.#"
                         "#E.G.E#"
                         "#.G.E.#"
                         "#######"]))))
(deftest initialise-battle-state-test
  (is (= (->BattleState ["#######"
                         "#.G.E.#"
                         "#E.G.E#"
                         "#.G.E.#"
                         "#######"]
                        {\E (zipmap [[1 4] [2 1] [2 5] [3 4]] (repeat starting-hp))
                         \G (zipmap [[1 2] [2 3] [3 2]] (repeat starting-hp))})
         (initialise-battle-state "#######
                                   #.G.E.#
                                   #E.G.E#
                                   #.G.E.#
                                   #######"))))

(deftest enemy-in-range-test
  (let [battle (->BattleState ["G...."
                               "..G.."
                               "..EG."
                               "..G.."
                               "...G."]
                              {\E {[2 2] starting-hp}
                               \G {[0 0] 9 [1 2] 4 [2 3] 2 [3 2] 2 [4 3] 1}})]
    (is (= nil (enemy-in-range battle [0 0] \E)))
    (is (= [2 2] (enemy-in-range battle [1 2] \E)))
    (is (= [2 3] (enemy-in-range battle [2 2] \G)))))

(deftest find-target-test
  (let [cavern ["#######"
                "#E..G.#"
                "#...#.#"
                "#.G.#G#"
                "#######"]]
    (is (= [[1 2] [1 3]] (find-target cavern [1 1] \G))))
  (let [cavern ["#######"
                "#.E...#"
                "#.....#"
                "#...G.#"
                "#######"]]
    (is (= [[1 3] [2 4]] (find-target cavern [1 2] \G)))))
