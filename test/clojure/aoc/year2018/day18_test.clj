(ns aoc.year2018.day18-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day18 :refer :all]))

(def input (slurp (clojure.java.io/resource "year2018/day18.txt")))

(deftest surrounding-tiles-test
  (is (= [[0 0] [0 1] [0 2]
          [1 0] [1 2]
          [2 0] [2 1] [2 2]] (surrounding-tiles [1 1]))))

(deftest count-surrounding-tile-states-test
  (let [area ["|#."
              "##|"
              ".#."]]
    (is (= {\| 2
            \# 3
            \. 3} (count-surrounding-tile-states area [1 1])))
    (is (= {\| 1
            \# 3
            \. 1} (count-surrounding-tile-states area [1 0])))
    (is (= {\# 3} (count-surrounding-tile-states area [2 0])))))

(deftest next-state-for-tile-test
  (testing "open area getting filled with trees"
    (is (= \| (next-state-for-tile \. {\| 3}))))
  (testing "open area remaining open"
    (is (= \. (next-state-for-tile \. {}))))
  (testing "trees becoming lumberyard"
    (is (= \# (next-state-for-tile \| {\# 3}))))
  (testing "trees remaining trees"
    (is (= \| (next-state-for-tile \| {}))))
  (testing "lumberyard remaining lumberyand"
    (is (= \# (next-state-for-tile \# {\# 1 \| 1}))))
  (testing "lumberyard becoming open area"
    (is (= \. (next-state-for-tile \# {})))))

(deftest next-state-for-area-test
  (is (= [[\. \. \. \. \. \. \. \# \# \.]
          [\. \. \. \. \. \. \| \# \# \#]
          [\. \| \. \. \| \. \. \. \# \.]
          [\. \. \| \# \| \| \. \. \. \#]
          [\. \. \# \# \| \| \. \| \# \|]
          [\. \. \. \# \| \| \| \| \. \.]
          [\| \| \. \. \. \| \| \| \. \.]
          [\| \| \| \| \| \. \| \| \. \|]
          [\| \| \| \| \| \| \| \| \| \|]
          [\. \. \. \. \| \| \. \. \| \.]]
         (next-state-for-area [[\. \# \. \# \. \. \. \| \# \.]
                               [\. \. \. \. \. \# \| \# \# \|]
                               [\. \| \. \. \| \. \. \. \# \.]
                               [\. \. \| \# \. \. \. \. \. \#]
                               [\# \. \# \| \| \| \# \| \# \|]
                               [\. \. \. \# \. \| \| \. \. \.]
                               [\. \| \. \. \. \. \| \. \. \.]
                               [\| \| \. \. \. \# \| \. \# \|]
                               [\| \. \| \| \| \| \. \. \| \.]
                               [\. \. \. \# \. \| \. \. \| \.]]))))

(deftest calculate-resource-value-test
  (is (= 1147 (calculate-resource-value [[\. \| \| \# \# \. \. \. \. \.]
                                         [\| \| \# \# \# \. \. \. \. \.]
                                         [\| \| \# \# \. \. \. \. \. \.]
                                         [\| \# \# \. \. \. \. \. \# \#]
                                         [\| \# \# \. \. \. \. \. \# \#]
                                         [\| \# \# \. \. \. \. \# \# \|]
                                         [\| \| \# \# \. \# \# \# \# \|]
                                         [\| \| \# \# \# \# \# \| \| \|]
                                         [\| \| \| \| \# \| \| \| \| \|]
                                         [\| \| \| \| \| \| \| \| \| \|]]))))

(deftest transduce-area-test
  (is (= [[\. \| \| \# \# \. \. \. \. \.]
          [\| \| \# \# \# \. \. \. \. \.]
          [\| \| \# \# \. \. \. \. \. \.]
          [\| \# \# \. \. \. \. \. \# \#]
          [\| \# \# \. \. \. \. \. \# \#]
          [\| \# \# \. \. \. \. \# \# \|]
          [\| \| \# \# \. \# \# \# \# \|]
          [\| \| \# \# \# \# \# \| \| \|]
          [\| \| \| \| \# \| \| \| \| \|]
          [\| \| \| \| \| \| \| \| \| \|]]
           (transduce-area 10 [[\. \# \. \# \. \. \. \| \# \.]
                               [\. \. \. \. \. \# \| \# \# \|]
                               [\. \| \. \. \| \. \. \. \# \.]
                               [\. \. \| \# \. \. \. \. \. \#]
                               [\# \. \# \| \| \| \# \| \# \|]
                               [\. \. \. \# \. \| \| \. \. \.]
                               [\. \| \. \. \. \. \| \. \. \.]
                               [\| \| \. \. \. \# \| \. \# \|]
                               [\| \. \| \| \| \| \. \. \| \.]
                               [\. \. \. \# \. \| \. \. \| \.]]))))
