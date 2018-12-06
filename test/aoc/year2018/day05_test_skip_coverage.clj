(ns aoc.year2018.day05-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day05 :refer :all]))

(def input (-> "year2018/day05.txt"
               (clojure.java.io/resource)
               (slurp)
               (clojure.string/trim)))

(deftest shortest-inert-length-removing-unit-test
  (is (= 4052 (shortest-inert-length-removing-unit input))))
