(ns aoc.year2018.day11-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day11 :refer :all]))

(deftest find-highest-power-region-test
  (is (= [113 [90 269 16]] (find-highest-power-region 300 18)))
  (is (= [119 [232 251 12]] (find-highest-power-region 300 42)))
  (is (= [119 [234 272 18]] (find-highest-power-region 300 8199))))
