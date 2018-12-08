(ns aoc.year2018.day08-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2018.day08 :refer :all]))

(def input (slurp (clojure.java.io/resource "year2018/day08.txt")))

(deftest parse-tree-test
  (is (= [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2] (parse-tree "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2\n"))))

(deftest sum-metadata-test
  (is (= 99 (sum-metadata [0 1 99])))
  (is (= [99 3] (sum-metadata 0 [0 1 99])))
  (is (= 33 (sum-metadata [0 3 10 11 12])))
  (is (= [33 5] (sum-metadata 0 [0 3 10 11 12])))
  (is (= [101 6] (sum-metadata 0 [1 1 0 1 99 2])))
  (is (= 101 (sum-metadata [1 1 0 1 99 2])))
  (is (= 138 (sum-metadata [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])))
  (is (= 40848 (sum-metadata (parse-tree input)))))
