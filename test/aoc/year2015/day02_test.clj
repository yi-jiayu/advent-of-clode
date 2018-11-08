(ns aoc.year2015.day02-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2015.day02 :refer :all]))

(def input (slurp (io/resource "year2015/day02.txt")))

(deftest test-parse-lwh
  (is (= '(2 3 4) (parse-lwh "2x3x4"))))

(deftest test-parse-input
  (is (= '((2 3 4) (1 1 10)) (parse-input "2x3x4\n1x1x10"))))

(deftest test-surface-area
  (is (= 52 (surface-area '(2 3 4))))
  (is (= 42 (surface-area '(1 1 10)))))

(deftest test-smallest-side
  (is (= 6 (smallest-side '(2 3 4))))
  (is (= 1 (smallest-side '(1 1 10)))))

(deftest test-wrapping-paper-required
  (is (= 58 (wrapping-paper-required '(2 3 4))))
  (is (= 43 (wrapping-paper-required '(1 1 10)))))

(deftest test-total-wrapping-paper-required
  (is (= 1606483 (total-wrapping-paper-required (parse-input input)))))
