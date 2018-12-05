(ns aoc.year2018.day05-test-skip-coverage
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day05 :refer :all]))

(def input (-> "year2018/day05.txt"
               (io/resource)
               (slurp)
               (clojure.string/trim)))


(deftest ^:slow inert-length-test
  (is (= 9238 (inert-length input))))

(deftest ^:slow shortest-inert-length-removing-unit-test
  (is (= 4052 (shortest-inert-length-removing-unit input))))
