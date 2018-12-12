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
