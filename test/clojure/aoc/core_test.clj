(ns aoc.core-test
  (:require [clojure.test :refer :all])
  (:require [aoc.core :refer :all]))

(deftest vadd-test
  (is (= [1 -1] (vadd [0 0] [1 -1])))
  (is (= [1 -1 3] (vadd [0 0 1] [1 -1 2]))))

(deftest adjacent-test
  (is (= [[0 1] [1 0] [1 2] [2 1]] (adjacent [1 1]))))

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
