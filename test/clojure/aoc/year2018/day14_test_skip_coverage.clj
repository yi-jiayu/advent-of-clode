(ns aoc.year2018.day14-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day14 :refer :all]))

(deftest last-m-recipes-after-making-n-test
  (is (= "2810862211" (apply str (last-m-recipes-after-making-n 509671 10)))))

(deftest scoreboard-until-specfic-recipe-sequence-test
  (is (= 20227889 (scoreboard-until-specfic-recipe-sequence [5 0 9 6 7 1]))))