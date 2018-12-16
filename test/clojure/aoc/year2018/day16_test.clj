(ns aoc.year2018.day16-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day16 :refer :all]))

(deftest addr-test
  (is (= [1 2 3 0] (addr [1 2 0 0] 0 1 2))))

(deftest addi-test
  (is (= [1 3 0 0] (addi [1 0 0 0] 0 2 1))))

(deftest banr-test
  (is (= [3 6 2 0] (banr [3 6 0 0] 0 1 2))))

(deftest bani-test
  (is (= [3 0 2 0] (bani [3 0 0 0] 0 6 2))))
