(ns aoc.year2017.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day11 :refer :all]))

(def input (slurp (io/resource "year2017/day11.txt")))

(deftest test-move
  (is (= [0 1] (move [0 0] :n))))

(deftest test-walk
  (is (= [0 0] (walk [:ne :ne :sw :sw]))))

(deftest test-distance
  (is (= 3 (distance (walk [:ne :ne :ne]))))
  (is (= 0 (distance (walk [:ne :ne :sw :sw]))))
  (is (= 2 (distance (walk [:ne :ne :s :s]))))
  (is (= 3 (distance (walk [:se :sw :se :sw :sw])))))

(deftest test-parse-input
  (is (= [:se :sw :se :sw :sw] (parse-input "se,sw,se,sw,sw"))))

(deftest test-fewest-steps-to-reach
  (testing "actual input"
    (is (= 675 (fewest-steps-to-reach (parse-input input))))))
