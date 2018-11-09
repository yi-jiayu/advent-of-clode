(ns aoc.year2017.day10-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day10 :refer :all]))

(def input "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63")

(deftest test-reverse-sublist
  (is (= [2 1 0 3 4] (reverse-sublist [0 1 2 3 4] 0 3)))
  (is (= [4 3 0 1 2] (reverse-sublist [2 1 0 3 4] 3 4)))
  (is (= [4 3 0 1 2] (reverse-sublist [4 3 0 1 2] 1 1)))
  (is (= [4 3 0 1 2] (reverse-sublist [3 4 2 1 0] 1 5))))

(deftest test-knot-hash-update
  (is (= [[2 1 0 3 4] 3 1] (knot-hash-update [[0 1 2 3 4] 0 0] 3)))
  (is (= [[4 3 0 1 2] 3 2] (knot-hash-update [[2 1 0 3 4] 3 1] 4)))
  (is (= [[4 3 0 1 2] 1 3] (knot-hash-update [[4 3 0 1 2] 3 2] 1)))
  (is (= [[3 4 2 1 0] 4 4] (knot-hash-update [[4 3 0 1 2] 1 3] 5))))

(deftest test-knot-hash
  (is (= 12 (knot-hash 5 [3 4 1 5])))
  (is (= 40132 (knot-hash (parse-input input)))))

(deftest test-parse-input
  (is (= [3 4 1 5] (parse-input "3,4,1,5"))))
