(ns aoc.year2018.day15-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day15 :refer :all]))

(def input (slurp (clojure.java.io/resource "year2018/day15.txt")))

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

(deftest find-target-in-range-test
  (let [battle (->Battle ["G...."
                          "..G.."
                          "..EG."
                          "..G.."
                          "...G."]
                         {[0 0] (->Unit \G 9),
                          [1 2] (->Unit \G 4),
                          [2 2] (->Unit \E 200),
                          [2 3] (->Unit \G 2),
                          [3 2] (->Unit \G 2),
                          [4 3] (->Unit \G 1)})]
    (is (= nil (find-target-in-range battle [0 0])))
    (is (= [2 2] (find-target-in-range battle [1 2])))
    (is (= [2 3] (find-target-in-range battle [2 2])))))

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

(deftest move-unit-test
  (let [battle (->Battle [[\# \# \# \# \# \# \#]
                          [\# \. \. \E \. \. \#]
                          [\# \. \E \. \. \. \#]
                          [\# \. \. \. \G \. \#]
                          [\# \# \# \# \# \# \#]]
                         {[1 3] (->Unit \E 200)
                          [2 2] (->Unit \E 200)
                          [3 4] (->Unit \G 200)})]
    (is (= (->Battle [[\# \# \# \# \# \# \#]
                      [\# \. \. \. \E \. \#]
                      [\# \. \E \. \. \. \#]
                      [\# \. \. \. \G \. \#]
                      [\# \# \# \# \# \# \#]]
                     {[1 4] (->Unit \E 200)
                      [2 2] (->Unit \E 200)
                      [3 4] (->Unit \G 200)}) (move-unit battle [1 3] [1 4])))))

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

(deftest attack-test
  (let [battle (->Battle [[\G \. \. \. \.]
                          [\. \. \G \. \.]
                          [\. \. \E \G \.]
                          [\. \. \G \. \.]
                          [\. \. \. \G \.]]
                         {[0 0] (->Unit \G 9),
                          [1 2] (->Unit \G 4),
                          [2 2] (->Unit \E 200),
                          [2 3] (->Unit \G 2),
                          [3 2] (->Unit \G 2),
                          [4 3] (->Unit \G 1)})]
    (is (= (->Battle [[\G \. \. \. \.]
                      [\. \. \G \. \.]
                      [\. \. \E \. \.]
                      [\. \. \G \. \.]
                      [\. \. \. \G \.]]
                     {[0 0] (->Unit \G 9),
                      [1 2] (->Unit \G 4),
                      [2 2] (->Unit \E 200),
                      [3 2] (->Unit \G 2),
                      [4 3] (->Unit \G 1)})
           (attack battle [2 3])))))

(deftest move-if-enemy-not-in-range-test
  (testing "should move if there are no enemy units in range"
    (let [battle (->Battle [[\# \# \# \# \# \# \#]
                            [\# \E \. \. \G \. \#]
                            [\# \. \. \. \# \. \#]
                            [\# \. \G \. \# \G \#]
                            [\# \# \# \# \# \# \#]]
                           {[1 1] (->Unit \E 200)
                            [1 4] (->Unit \G 200)
                            [3 2] (->Unit \G 200)
                            [3 5] (->Unit \G 200)})]
      (is (= [(->Battle [[\# \# \# \# \# \# \#]
                         [\# \. \E \. \G \. \#]
                         [\# \. \. \. \# \. \#]
                         [\# \. \G \. \# \G \#]
                         [\# \# \# \# \# \# \#]]
                        {[1 2] (->Unit \E 200)
                         [1 4] (->Unit \G 200)
                         [3 2] (->Unit \G 200)
                         [3 5] (->Unit \G 200)})
              [1 2]] (move-if-enemy-not-in-range battle [1 1])))))
  (testing "should not move if enemy unit is already in range"
    (let [battle (->Battle [[\# \# \# \# \# \# \#]
                            [\# \. \. \E \G \. \#]
                            [\# \. \. \. \# \. \#]
                            [\# \. \G \. \# \G \#]
                            [\# \# \# \# \# \# \#]]
                           {[1 3] (->Unit \E 200)
                            [1 4] (->Unit \G 200)
                            [3 2] (->Unit \G 200)
                            [3 5] (->Unit \G 200)})]
      (is (= [(->Battle [[\# \# \# \# \# \# \#]
                         [\# \. \. \E \G \. \#]
                         [\# \. \. \. \# \. \#]
                         [\# \. \G \. \# \G \#]
                         [\# \# \# \# \# \# \#]]
                        {[1 3] (->Unit \E 200)
                         [1 4] (->Unit \G 200)
                         [3 2] (->Unit \G 200)
                         [3 5] (->Unit \G 200)})
              [1 3]] (move-if-enemy-not-in-range battle [1 3]))))))

(deftest attack-if-enemy-in-range-test
  (testing "should attack enemy when in range"
    (let [battle (->Battle [[\. \. \. \. \.]
                            [\. \. \G \. \.]
                            [\. \. \E \G \.]
                            [\. \. \G \. \.]
                            [\. \. \. \. \.]]
                           {[1 2] (->Unit \G 200)
                            [2 2] (->Unit \E 200)
                            [2 3] (->Unit \G 5)
                            [3 2] (->Unit \G 200)})]
      (is (= (->Battle [[\. \. \. \. \.]
                        [\. \. \G \. \.]
                        [\. \. \E \G \.]
                        [\. \. \G \. \.]
                        [\. \. \. \. \.]]
                       {[1 2] (->Unit \G 200)
                        [2 2] (->Unit \E 200)
                        [2 3] (->Unit \G 2)
                        [3 2] (->Unit \G 200)})
             (attack-if-enemy-in-range [battle [2 2]])))))
  (testing "should not attack when no enemies are in range"
    (let [battle (->Battle [[\. \. \. \. \.]
                            [\. \. \G \. \.]
                            [\. \E \. \G \.]
                            [\. \. \G \. \.]
                            [\. \. \. \. \.]]
                           {[1 2] (->Unit \G 200)
                            [2 1] (->Unit \E 200)
                            [2 3] (->Unit \G 5)
                            [3 2] (->Unit \G 200)})]
      (is (= (->Battle [[\. \. \. \. \.]
                        [\. \. \G \. \.]
                        [\. \E \. \G \.]
                        [\. \. \G \. \.]
                        [\. \. \. \. \.]]
                       {[1 2] (->Unit \G 200)
                        [2 1] (->Unit \E 200)
                        [2 3] (->Unit \G 5)
                        [3 2] (->Unit \G 200)})
             (attack-if-enemy-in-range [battle [2 1]]))))))

(deftest take-turn-test
  (let [battle (parse-input "#######\n#.E...#\n#.....#\n#...G.#\n#######")]
    (is (= (->Battle [[\# \# \# \# \# \# \#]
                      [\# \. \. \E \. \. \#]
                      [\# \. \. \. \. \. \#]
                      [\# \. \. \. \G \. \#]
                      [\# \# \# \# \# \# \#]]
                     {[1 3] (->Unit \E 200)
                      [3 4] (->Unit \G 200)})
           (take-turn battle [1 2]))))
  (let [battle (->Battle [[\# \# \# \# \# \# \#]
                          [\# \. \. \E \. \. \#]
                          [\# \. \. \. \. \. \#]
                          [\# \. \. \. \G \. \#]
                          [\# \# \# \# \# \# \#]]
                         {[1 3] (->Unit \E 200)
                          [3 4] (->Unit \G 200)})]
    (is (= (->Battle [[\# \# \# \# \# \# \#]
                      [\# \. \. \E \. \. \#]
                      [\# \. \. \. \G \. \#]
                      [\# \. \. \. \. \. \#]
                      [\# \# \# \# \# \# \#]]
                     {[1 3] (->Unit \E 200)
                      [2 4] (->Unit \G 200)})
           (take-turn battle [3 4])))
    (testing "returns nil when the battle is over"
      (let [battle (parse-input "#######\n#G....#\n#.G...#\n#.#.#G#\n#...#.#\n#....G#\n#######")]
        (is (nil? (take-turn battle [1 1])))))))

(deftest take-all-turns-test
  (let [round0 (->Battle [[\# \# \# \# \# \# \# \# \#]
                          [\# \G \. \. \G \. \. \G \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \G \. \. \E \. \. \G \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \G \. \. \G \. \. \G \#]
                          [\# \# \# \# \# \# \# \# \#]]
                         (into (sorted-map) {[1 1] (->Unit \G 200),
                                             [1 4] (->Unit \G 200),
                                             [1 7] (->Unit \G 200),
                                             [4 1] (->Unit \G 200),
                                             [4 4] (->Unit \E 200),
                                             [4 7] (->Unit \G 200),
                                             [7 1] (->Unit \G 200),
                                             [7 4] (->Unit \G 200),
                                             [7 7] (->Unit \G 200)}))
        round1 (->Battle [[\# \# \# \# \# \# \# \# \#]
                          [\# \. \G \. \. \. \G \. \#]
                          [\# \. \. \. \G \. \. \. \#]
                          [\# \. \. \. \E \. \. \G \#]
                          [\# \. \G \. \. \. \. \. \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \G \. \. \G \. \. \G \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \# \# \# \# \# \# \# \#]]
                         (into (sorted-map) {[1 2] (->Unit \G 200),
                                             [1 6] (->Unit \G 200),
                                             [2 4] (->Unit \G 197),
                                             [3 4] (->Unit \E 200),
                                             [3 7] (->Unit \G 200),
                                             [4 2] (->Unit \G 200),
                                             [6 1] (->Unit \G 200),
                                             [6 4] (->Unit \G 200),
                                             [6 7] (->Unit \G 200)}))
        round2 (->Battle [[\# \# \# \# \# \# \# \# \#]
                          [\# \. \. \G \. \G \. \. \#]
                          [\# \. \. \. \G \. \. \. \#]
                          [\# \. \G \. \E \. \G \. \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \G \. \. \G \. \. \G \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \. \. \. \. \. \. \. \#]
                          [\# \# \# \# \# \# \# \# \#]],
                         {[1 3] (->Unit \G 200),
                          [1 5] (->Unit \G 200),
                          [2 4] (->Unit \G 194),
                          [3 2] (->Unit \G 200),
                          [3 4] (->Unit \E 197),
                          [3 6] (->Unit \G 200),
                          [5 1] (->Unit \G 200),
                          [5 4] (->Unit \G 200),
                          [5 7] (->Unit \G 200)})
        ]
    (is (= [round1 false] (take-all-turns round0)))
    (is (= [round2 false] (take-all-turns round1)))
    (testing "final round"
      (let [final-round (->Battle [[\# \# \# \# \# \# \#]
                                   [\# \G \. \. \. \. \#]
                                   [\# \. \G \. \. \. \#]
                                   [\# \. \# \. \# \G \#]
                                   [\# \. \. \. \# \. \#]
                                   [\# \. \. \. \. \G \#]
                                   [\# \# \# \# \# \# \#]]
                                  {[1 1] (->Unit \G 200),
                                   [2 2] (->Unit \G 131),
                                   [3 5] (->Unit \G 59),
                                   [5 5] (->Unit \G 200)})]
        (is (= [final-round true] (take-all-turns final-round)))))))

(deftest calculate-outcome-test
  (is (= 27730 (calculate-outcome {:units {[1 1] (->Unit \G 200),
                                           [2 2] (->Unit \G 131),
                                           [3 5] (->Unit \G 59),
                                           [5 5] (->Unit \G 200)}} 47))))

(deftest simulate-battle-test
  (let [battle (parse-input "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")]
    (is (= 27730 (simulate-battle battle))))
  (let [battle (parse-input "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######")]
    (is (= 36334 (simulate-battle battle))))
  (let [battle (parse-input "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######")]
    (is (= 39514 (simulate-battle battle))))
  ;(let [battle (parse-input "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######")]
  ;  (is (= 27755 (simulate-battle battle))))
  (let [battle (parse-input "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######")]
    (is (= 28944 (simulate-battle battle))))
  (let [battle (parse-input "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########")]
    (is (= 18740 (simulate-battle battle))))
  ;(let [battle (parse-input input)]
  ;  (is (= 0 (simulate-battle battle))))
  )
