(ns aoc.year2018.day06-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day06 :refer :all]))

(def input (-> "year2018/day06.txt"
               (clojure.java.io/resource)
               (slurp)))

(deftest largest-area-test
  (is (= 3449 (largest-area (parse-coords input)))))

(deftest safest-region-size-test
  (is (= 44868 (safest-region-size 10000 (parse-coords input)))))
