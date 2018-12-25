(ns aoc.year2018.day18-test-skip-coverage
  (:require [clojure.test :refer :all]
            [aoc.year2018.day18 :refer :all]))

(def input (slurp (clojure.java.io/resource "year2018/day18.txt")))

(deftest ^:slow find-first-repetition-and-cycle-time-test-slow
  (is (= [489 28] (find-first-repetition-and-cycle-time (parse-input input)))))

(deftest ^:slow transduce-optimised-test-slow
  (let [area (parse-input input)]
    (is (= 208080 (-> area
                      (transduce-optimised 1000000000)
                      (calculate-resource-value))))))
