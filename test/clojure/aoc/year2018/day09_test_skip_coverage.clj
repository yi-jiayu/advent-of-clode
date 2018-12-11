(ns aoc.year2018.day09-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day09 :refer :all]))

(deftest play-marble-game-test
  (is (= 3426843186 (apply max (vals (play-marble-game 418 7076900))))))
