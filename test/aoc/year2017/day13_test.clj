(ns aoc.year2017.day13-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day13 :refer :all]))

(def example-input "0: 3\n1: 2\n4: 4\n6: 4")
(def input (slurp (io/resource "year2017/day13.txt")))

(deftest initialise-firewall2-test
  (is (= [[0 3] [1 2] [4 4] [6 4]] (initialise-firewall example-input))))

(deftest scanner-position-test
  (is (= 0 (scanner-position 4 0)))
  (is (= 3 (scanner-position 4 3)))
  (is (= 1 (scanner-position 4 5)))
  (is (= 0 (scanner-position 4 6)))
  (is (= 1 (scanner-position 4 7))))

(deftest severity-test
  (is (= 24 (severity [6 4]))))

(deftest total-severity-test
  (is (= 24 (total-severity (initialise-firewall example-input))))
  (is (= 632 (total-severity (initialise-firewall input)))))

(deftest not-caught?-test
  (is (not (not-caught? (initialise-firewall example-input) 0)))
  (is (not-caught? (initialise-firewall example-input) 10)))

(deftest find-minimum-delay-test
  (is (= 10 (find-minimum-delay (initialise-firewall example-input)))))

(deftest ^:slow find-minimum-delay-long-test
  (is (= 3849742 (find-minimum-delay (initialise-firewall input)))))
