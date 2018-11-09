(ns aoc.year2017.day13-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day13 :refer :all]))

(def example-input "0: 3\n1: 2\n4: 4\n6: 4")
(def input (slurp (io/resource "year2017/day13.txt")))

(deftest test-initialise-firewall2
  (is (= [[0 3] [1 2] [4 4] [6 4]] (initialise-firewall example-input))))

(deftest test-scanner-position
  (is (= 0 (scanner-position 4 0)))
  (is (= 3 (scanner-position 4 3)))
  (is (= 1 (scanner-position 4 5)))
  (is (= 0 (scanner-position 4 6)))
  (is (= 1 (scanner-position 4 7))))

(deftest test-severity
  (is (= 24 (severity [6 4]))))

(deftest test-total-severity
  (is (= 24 (total-severity (initialise-firewall example-input))))
  (is (= 632 (total-severity (initialise-firewall input)))))

(deftest test-not-caught?
  (is (not (not-caught? (initialise-firewall example-input) 0)))
  (is (not-caught? (initialise-firewall example-input) 10)))

(deftest test-find-minimum-delay
  (is (= 10 (find-minimum-delay (initialise-firewall example-input))))
  (is (= 3849742 (find-minimum-delay (initialise-firewall input)))))
