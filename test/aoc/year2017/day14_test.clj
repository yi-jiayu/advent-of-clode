(ns aoc.year2017.day14-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day14 :refer :all]))

(def example-input "flqrgnkx")
(def input "jxqlasbh")

(deftest test-to-bin
  (is (clojure.string/starts-with?
        (to-bin "a0c2017")
        "1010000011000010000000010111")))

(deftest test-used-squares
  (is (= 8108 (used-squares (to-grid example-input))))
  (is (= 8140 (used-squares (to-grid input)))))
