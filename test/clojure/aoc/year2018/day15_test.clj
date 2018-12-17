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
  (is (= [[\# \# \# \# \# \# \#]
          [\# \. \G \. \E \. \#]
          [\# \E \. \G \. \E \#]
          [\# \. \G \. \E \. \#]
          [\# \# \# \# \# \# \#]]
         (parse-cavern-map "#######
                            #.G.E.#
                            #E.G.E#
                            #.G.E.#
                            #######"))))

(deftest extract-units-test
  (is (= {[1 2] (->Unit \G 200)
          [1 4] (->Unit \E 200)
          [2 1] (->Unit \E 200)
          [2 3] (->Unit \G 200)
          [2 5] (->Unit \E 200)
          [3 2] (->Unit \G 200)
          [3 4] (->Unit \E 200)}
         (extract-units ["#######"
                         "#.G.E.#"
                         "#E.G.E#"
                         "#.G.E.#"
                         "#######"]))))
(deftest parse-input-test
  (is (= (->Battle [[\# \# \# \# \# \# \#]
                    [\# \. \G \. \E \. \#]
                    [\# \E \. \G \. \E \#]
                    [\# \. \G \. \E \. \#]
                    [\# \# \# \# \# \# \#]]
                   {[1 2] (->Unit \G 200)
                    [1 4] (->Unit \E 200)
                    [2 1] (->Unit \E 200)
                    [2 3] (->Unit \G 200)
                    [2 5] (->Unit \E 200)
                    [3 2] (->Unit \G 200)
                    [3 4] (->Unit \E 200)})
         (parse-input "#######
                                   #.G.E.#
                                   #E.G.E#
                                   #.G.E.#
                                   #######"))))

(deftest identify-targets-test
  (let [units {[1 1] (->Unit \E 200)
               [1 4] (->Unit \G 200)
               [3 2] (->Unit \G 200)
               [3 5] (->Unit \G 200)}]
    (is (= [[1 4] [3 2] [3 5]] (identify-targets {:units units} [1 1]))))
  (testing "no targets remaining"
    (let [units {[1 1] (->Unit \E 200)}]
      (is (= nil (identify-targets {:units units} [1 1]))))))

(deftest identify-open-squares-test
  (let [cavern [[\# \# \# \# \# \# \#]
                [\# \E \. \. \G \. \#]
                [\# \. \. \. \# \. \#]
                [\# \. \G \. \# \G \#]
                [\# \# \# \# \# \# \#]]]
    (is (= #{[1 3] [1 5] [2 2] [2 5] [3 1] [3 3]}
           (identify-open-squares {:cavern cavern} [[1 4] [3 2] [3 5]]))))
  (testing "no open squares"
    (let [cavern [[\# \# \# \# \# \#]
                  [\# \E \. \E \G \#]
                  [\# \. \. \# \G \#]
                  [\# \# \# \# \# \#]]]
      (is (= nil
             (identify-open-squares {:cavern cavern} [[1 4] [2 4]]))))))

(deftest choose-next-step-test
  (let [cavern ["#######"
                "#.E...#"
                "#.....#"
                "#...G.#"
                "#######"]]
    (is (= [1 3] (choose-next-step {:cavern cavern} [1 2] #{[3 3] [2 4] [3 5]}))))
  (let [cavern ["#######"
                "#.G...#"
                "#...E.#"
                "#.G...#"
                "#######"]]
    (is (= [1 4] (choose-next-step {:cavern cavern} [2 4] #{[2 2] [3 3] [1 1]
                                                            [1 3] [3 1]}))))
  (testing "should return nil when no targets are reachable"
    (let [cavern ["#######"
                  "#E..E.#"
                  "#...#.#"
                  "#...#G#"
                  "#######"]]
      (is (= nil (choose-next-step {:cavern cavern} [1 1] #{[2 5]}))))))

;(deftest enemy-in-range-test
;  (let [battle (->Battle ["G...."
;                          "..G.."
;                          "..EG."
;                          "..G.."
;                          "...G."]
;                         {[0 0] (->Unit \G 200),
;                          [1 2] (->Unit \G 200),
;                          [2 2] (->Unit \E 200),
;                          [2 3] (->Unit \G 200),
;                          [3 2] (->Unit \G 200),
;                          [4 3] (->Unit \G 200)})]
;    (is (= nil (enemy-in-range battle [0 0] \E)))
;    (is (= [2 2] (enemy-in-range battle [1 2] \E)))
;    (is (= [2 3] (enemy-in-range battle [2 2] \G)))))

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
    (is (= [[1 3] [2 4]] (find-target cavern [1 2] \G))))
  (let [cavern ["#######"
                "#.G...#"
                "#...E.#"
                "#.G...#"
                "#######"]]
    (is (= [[1 4] [1 3]] (find-target cavern [2 4] \G))))
  (testing "should return nil when no targets are reachable"
    (let [cavern ["#######"
                  "#E..E.#"
                  "#...#.#"
                  "#...#G#"
                  "#######"]]
      (is (= nil (find-target cavern [1 1] \G))))))

(deftest move-unit-test
  (let [battle (->Battle
                 [[\# \# \# \# \# \# \#]
                  [\# \. \. \E \. \. \#]
                  [\# \. \E \. \. \. \#]
                  [\# \. \. \. \G \. \#]
                  [\# \# \# \# \# \# \#]]
                 {\E {[1 3] 200
                      [2 2] 190}
                  \G {[3 4] 200}})]
    (is (= (->Battle
             [[\# \# \# \# \# \# \#]
              [\# \. \. \. \E \. \#]
              [\# \. \E \. \. \. \#]
              [\# \. \. \. \G \. \#]
              [\# \# \# \# \# \# \#]]
             {\E {[1 4] 200
                  [2 2] 190}
              \G {[3 4] 200}}) (move-unit battle [1 3] [1 4])))))

;(deftest simulate-battle-test
;  (let [battle (initialise-battle-state "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")]
;    (is (= 27730 (simulate-battle battle)))))

(deftest attack-test
  (let [battle (->Battle [[\G \. \. \. \.]
                          [\. \. \G \. \.]
                          [\. \. \E \G \.]
                          [\. \. \G \. \.]
                          [\. \. \. \G \.]]
                         {\E {[2 2] starting-hp}
                          \G {[0 0] 9 [1 2] 4 [2 3] 2 [3 2] 2 [4 3] 1}})]
    (is (= (->Battle [[\G \. \. \. \.]
                      [\. \. \G \. \.]
                      [\. \. \E \. \.]
                      [\. \. \G \. \.]
                      [\. \. \. \G \.]]
                     {\E {[2 2] starting-hp}
                      \G {[0 0] 9 [1 2] 4 [3 2] 2 [4 3] 1}})
           (attack battle [2 2] 3 [2 3])))))

(deftest move-if-enemy-not-in-range-test
  )

;(deftest take-turn-test
;  (let [battle (parse-input "#######\n#.E...#\n#.....#\n#...G.#\n#######")]
;    (is (= (->Battle
;             [[\# \# \# \# \# \# \#]
;              [\# \. \. \E \. \. \#]
;              [\# \. \. \. \. \. \#]
;              [\# \. \. \. \G \. \#]
;              [\# \# \# \# \# \# \#]]
;             {\E {[1 3] 200}
;              \G {[3 4] 200}})
;           (take-turn battle [1 2])))))
