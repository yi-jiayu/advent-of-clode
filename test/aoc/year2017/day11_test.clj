(ns aoc.year2017.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day11 :refer :all]))

(def input (slurp (io/resource "year2017/day11.txt")))

(deftest test-move
  (is (= [0 1] (move [0 0] :n))))

(deftest test-walk-from-origin
  (is (= [0 0] (walk-from-origin [:ne :ne :sw :sw]))))

(deftest test-distance
  (is (= 3 (distance [3 0])))
  (is (= 0 (distance [0 0])))
  (is (= 2 (distance [2 -2])))
  (is (= 3 (distance [-1 -2]))))

(deftest test-parse-input
  (is (= [:se :sw :se :sw :sw] (parse-input "se,sw,se,sw,sw"))))

(deftest test-fewest-steps-to-reach
  (is (= 675 (fewest-steps-to-reach (parse-input input)))))

(deftest test-max-distance-from-origin
  (is (= 1424 (max-distance-from-origin (parse-input input)))))
