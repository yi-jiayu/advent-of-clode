(ns aoc.year2017.day01-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day01 :refer :all]))

(def input (slurp (io/resource "year2017/day01.txt")))

(deftest test-solve-captcha-part-1
  (testing "example inputs"
    (is (= 3 (solve-captcha-part-1 "1122")))
    (is (= 4 (solve-captcha-part-1 "1111")))
    (is (= 0 (solve-captcha-part-1 "1234")))
    (is (= 9 (solve-captcha-part-1 "91212129"))))
  (testing "actual input"
    (is (= 1253 (solve-captcha-part-1 input)))))

(deftest test-solve-captcha-part-2
  (testing "example inputs"
    (is (= 6 (solve-captcha-part-2 "1212")))
    (is (= 0 (solve-captcha-part-2 "1221")))
    (is (= 4 (solve-captcha-part-2 "123425")))
    (is (= 12 (solve-captcha-part-2 "123123")))
    (is (= 4 (solve-captcha-part-2 "12131415"))))
  (testing "actual input"
    (is (= 1278 (solve-captcha-part-2 input)))))
