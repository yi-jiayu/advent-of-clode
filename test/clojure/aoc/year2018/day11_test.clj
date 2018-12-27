(ns aoc.year2018.day11-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day11 :refer :all]))

(deftest get-hundreds-digit-test
  (is (= 0 (get-hundreds-digit 99)))
  (is (= 1 (get-hundreds-digit 100))))

(deftest calculate-power-level-test
  (is (= 4 (calculate-power-level 8 3 5)))
  (is (= -5 (calculate-power-level 57 122 79)))
  (is (= 0 (calculate-power-level 39 217 196)))
  (is (= 4 (calculate-power-level 71 101 153))))

(deftest partition-multiple-test
  (let [colls [[0 1 2]
               [3 4 5]]]
    (is (= [[0 1 3 4]
            [1 2 4 5]] (partition-multiple 2 1 colls)))))

(deftest partition2d-test
  (let [matrix [[0 1 2]
                [3 4 5]
                [6 7 8]]]
    (is (= '((0 1 3 4)
              (1 2 4 5)
              (3 4 6 7)
              (4 5 7 8))
           (partition2d 2 1 matrix))))
  (let [matrix [[0 1 2 3]
                [4 5 6 7]
                [8 9 10 11]]]
    (is (= '((0 1 4 5)
              (1 2 5 6)
              (2 3 6 7)
              (4 5 8 9)
              (5 6 9 10)
              (6 7 10 11))
           (partition2d 2 1 matrix)))))

(deftest convolve-test
  (let [matrix [[0 1 2]
                [3 4 5]
                [6 7 8]]]
    (is (= [8 12 20 24]
           (convolve (partial apply +)
                     2
                     1
                     matrix)))))

(deftest index-to-coord-test
  (is (= [0 0] (index-to-coord 3 4 2 1 0)))
  (is (= [0 1] (index-to-coord 3 4 2 1 1)))
  (is (= [1 0] (index-to-coord 3 4 2 1 3)))
  (is (= [1 1] (index-to-coord 3 4 2 1 4))))

(deftest calculate-power-levels-test
  (is (= [[4 -5 -4]
          [-4 -2 -1]
          [-2 0 1]]
         (calculate-power-levels 3 8199)))
  (is (= [[4 -5 -4 -3]
          [-4 -2 -1 0]
          [-2 0 1 3]
          [0 2 4 -5]]
         (calculate-power-levels 4 8199))))

(deftest find-highest-power-3x3-region-test
  (is (= [2 [1 1]] (find-highest-power-3x3-region 4 8199)))
  (is (= [30 [21 61]] (find-highest-power-3x3-region 300 42)))
  (is (= [28 [235 87]] (find-highest-power-3x3-region 300 8199))))

(deftest build-summed-area-table-test
  (is (= [[4 -1 -5 -8]
          [0 -7 -12 -15]
          [-2 -9 -13 -13]
          [-2 -7 -7 -12]]
         (build-summed-area-table 4 8199))))

(deftest calculate-region-total-power-test
  (let [table [[4 -1 -5 -8]
               [0 -7 -12 -15]
               [-2 -9 -13 -13]
               [-2 -7 -7 -12]]]
    (is (= -7 (calculate-region-total-power table 0 0 2)))
    (is (= -13 (calculate-region-total-power table 0 0 3)))
    (is (= -2 (calculate-region-total-power table 1 1 2)))))

(deftest find-highest-power-region-test
  (is (= [12 [0 4 3]] (find-highest-power-region 10 18))))
