(ns aoc.year2017.day01-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day01 :refer :all]))

(def input (slurp (io/resource "year2017/day01.txt")))

(deftest test-solve-captcha
  (testing "example inputs"
    (is (= 3 (solve-captcha "1122")))
    (is (= 4 (solve-captcha "1111")))
    (is (= 0 (solve-captcha "1234")))
    (is (= 9 (solve-captcha "91212129"))))
  (testing "actual input"
    (is (= 1253 (solve-captcha input)))))
