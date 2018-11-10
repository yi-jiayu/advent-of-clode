(ns aoc.year2015.day01-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2015.day01 :refer :all]))

(def input (slurp (io/resource "year2015/day01.txt")))

(deftest final-floor-test
  (testing "example inputs"
    (is (= 0 (final-floor "(())")))
    (is (= 0 (final-floor "()()")))
    (is (= 3 (final-floor "(((")))
    (is (= 3 (final-floor "(()(()(")))
    (is (= 3 (final-floor "))(((((")))
    (is (= -1 (final-floor "())")))
    (is (= -1 (final-floor "))(")))
    (is (= -3 (final-floor ")))")))
    (is (= -3 (final-floor ")())())"))))
  (testing "actual input"
    (is (= 280 (final-floor input)))))

(deftest first-step-into-basement-test
  (testing "example inputs"
    (is (= 1 (first-step-into-basement ")")))
    (is (= 5 (first-step-into-basement "()())"))))
  (testing "actual input"
    (is (= 1797 (first-step-into-basement input)))))
