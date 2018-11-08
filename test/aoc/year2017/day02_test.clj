(ns aoc.year2017.day02-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day02 :refer :all]))

(def input (slurp (io/resource "year2017/day02.txt")))

(deftest test-parse-row
  (is (= '(5 9 2 8) (parse-row "5 9 2 8")))
  (is (= '(493 458 321 120) (parse-row "493	458	321	120"))))

(deftest test-parse-spreadsheet
  (is (= '((5 9 2 8) (9 4 7 3) (3 8 6 5)) (parse-spreadsheet "5 9 2 8\n9 4 7 3\n3 8 6 5"))))

(deftest test-line-checksum
  (is (= 8 (line-checksum '(5 1 9 5)))))

(deftest test-spreadsheet-checksum
  (is (= 18 (spreadsheet-checksum '((5 1 9 5) (7 5 3) (2 4 6 8)))))
  (testing "actual input"
    (is (= 21845 (spreadsheet-checksum (parse-spreadsheet input))))))

(deftest test-find-result
  (is (= 4 (find-result '(5 9 2 8))))
  (is (= 3 (find-result '(9 4 7 3))))
  (is (= 2 (find-result '(3 8 6 5)))))

(deftest test-sum-results
  (is (= 9 (sum-results '((5 9 2 8) (9 4 7 3) (3 8 6 5)))))
  (testing "actual input"
    (is (= 191 (sum-results (parse-spreadsheet input))))))
