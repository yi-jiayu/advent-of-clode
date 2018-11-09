(ns aoc.year2017.day13-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day13 :refer :all]))

(def example-input "0: 3\n1: 2\n4: 4\n6: 4")
(def input (slurp (io/resource "year2017/day13.txt")))

(deftest test-initialise-firewall
  (is (= [[3 0] [2 0] [0 0] [0 0] [4 0] [0 0] [4 0]] (initialise-firewall example-input))))

(deftest test-scanner-position
  (is (= 0 (scanner-position 4 0)))
  (is (= 3 (scanner-position 4 3)))
  (is (= 1 (scanner-position 4 5)))
  (is (= 0 (scanner-position 4 6)))
  (is (= 1 (scanner-position 4 7))))

(deftest test-calculate-scanner-positions
  (is (= [[3 1] [2 1] [0 0] [0 0] [4 1] [0 0] [4 1]]
         (calculate-scanner-positions [[3 0] [2 0] [0 0] [0 0] [4 0] [0 0] [4 0]] 1)))
  (is (= [[3 2] [2 0] [0 0] [0 0] [4 2] [0 0] [4 2]]
         (calculate-scanner-positions [[3 1] [2 1] [0 0] [0 0] [4 1] [0 0] [4 1]] 2))))

(deftest test-calculate-severity
  (is (= 24 (calculate-severity (initialise-firewall example-input))))
  (is (= 632 (calculate-severity (initialise-firewall input)))))
