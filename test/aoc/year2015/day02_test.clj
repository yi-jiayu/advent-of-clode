(ns aoc.year2015.day02-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2015.day02 :refer :all]))

(def input (slurp (io/resource "year2015/day02.txt")))

(deftest parse-lwh-test
  (is (= '(2 3 4) (parse-lwh "2x3x4"))))

(deftest parse-input-test
  (is (= '((2 3 4) (1 1 10)) (parse-input "2x3x4\n1x1x10"))))

(deftest surface-area-test
  (is (= 52 (surface-area '(2 3 4))))
  (is (= 42 (surface-area '(1 1 10)))))

(deftest smallest-side-test
  (is (= 6 (smallest-side '(2 3 4))))
  (is (= 1 (smallest-side '(1 1 10)))))

(deftest wrapping-paper-required-test
  (is (= 58 (wrapping-paper-required '(2 3 4))))
  (is (= 43 (wrapping-paper-required '(1 1 10)))))

(deftest total-wrapping-paper-required-test
  (is (= 1606483 (total-wrapping-paper-required (parse-input input)))))
