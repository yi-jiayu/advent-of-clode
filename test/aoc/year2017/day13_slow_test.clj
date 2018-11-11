(ns aoc.year2017.day13-slow-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day13 :refer :all]))

(def input (slurp (io/resource "year2017/day13.txt")))

(deftest ^:slow find-minimum-delay-long-test
  (is (= 3849742 (find-minimum-delay (initialise-firewall input)))))
