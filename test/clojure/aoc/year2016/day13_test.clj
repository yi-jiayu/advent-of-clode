(ns aoc.year2016.day13-test
  (:require [clojure.test :refer :all]
            [aoc.year2016.day13 :refer :all]))

(def input 1352)

(deftest count-set-bits-test
  (is (= 2 (count-set-bits 6)))
  (is (= 3 (count-set-bits 11))))

(deftest open?-test
  (testing "example inputs"
    (let [fav 10]
      (is (open? fav [0 0]))
      (is (not (open? fav [2 1]))))))

(deftest adjacent-test
  (is (= [[2 1] [0 1] [1 2] [1 0]] (adjacent [1 1]))))

(deftest first-quadrant?-test
  (is (first-quadrant? [1 1]))
  (is (first-quadrant? [0 0]))
  (is (not (first-quadrant? [-1 1]))))

(deftest neighbours-test
  (let [fav 10]
    (is (= [[0 1] [1 2]] (neighbours fav [1 1])))))

(deftest path-to-test
  (is (= 11 (steps-to 10 [7 4])))
  (testing "actual input"
    (is (= 90 (steps-to input [31 39])))))

(deftest explore-test
  (testing "actual input"
    (is (= 135 (explore input 50)))))
