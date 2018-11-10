(ns aoc.year2017.day11-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day11 :refer :all]))

(def input (slurp (io/resource "year2017/day11.txt")))

(deftest move-test
  (is (= [0 1] (move [0 0] :n))))

(deftest walk-from-origin-test
  (is (= [0 0] (walk-from-origin [:ne :ne :sw :sw]))))

(deftest distance-test
  (is (= 3 (distance [3 0])))
  (is (= 0 (distance [0 0])))
  (is (= 2 (distance [2 -2])))
  (is (= 3 (distance [-1 -2]))))

(deftest parse-input-test
  (is (= [:se :sw :se :sw :sw] (parse-input "se,sw,se,sw,sw"))))

(deftest fewest-steps-to-reach-test
  (is (= 675 (fewest-steps-to-reach (parse-input input)))))

(deftest max-distance-from-origin-test
  (is (= 1424 (max-distance-from-origin (parse-input input)))))
