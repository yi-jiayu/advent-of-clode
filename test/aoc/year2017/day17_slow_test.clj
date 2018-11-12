(ns aoc.year2017.day17-slow-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day17 :refer :all]))

(def input 345)

(deftest track-value-after-zero-slow-test
  (is (= 11995607 (track-value-after-zero 5E7 input))))
