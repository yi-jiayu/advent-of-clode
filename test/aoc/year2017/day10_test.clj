(ns aoc.year2017.day10-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day10 :refer :all]))

(deftest test-reverse-sublist
  (is (= [2 1 0 3 4] (reverse-sublist [0 1 2 3 4] 0 3)))
  (is (= [4 3 0 1 2] (reverse-sublist [2 1 0 3 4] 3 4)))
  (is (= [4 3 0 1 2] (reverse-sublist [4 3 0 1 2] 1 1)))
  (is (= [4 3 0 1 2] (reverse-sublist [3 4 2 1 0] 1 5))))
