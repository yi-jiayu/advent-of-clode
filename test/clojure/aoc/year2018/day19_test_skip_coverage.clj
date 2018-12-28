(ns aoc.year2018.day19-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day19 :refer :all]))

(def input (slurp (clojure.java.io/resource "year2018/day19.txt")))

(deftest run-program-test
  (is (= {:ip         257
          :ip-binding 4
          :registers  [912 911 912 1 256 912]} (run-program (parse-input input)))))

(deftest factorise-test
  (is (= 10576224 (apply + (factorise 10551311)))))
