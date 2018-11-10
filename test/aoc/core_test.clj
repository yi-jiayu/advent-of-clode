(ns aoc.core-test
  (:require [clojure.test :refer :all])
  (:require [aoc.core :refer :all]))

(deftest vadd-test
  (is (= [1 -1] (vadd [0 0] [1 -1])))
  (is (= [1 -1 3] (vadd [0 0 1] [1 -1 2]))))

(deftest adjacent-test
  (is (= [[2 1] [0 1] [1 2] [1 0]] (adjacent [1 1]))))
