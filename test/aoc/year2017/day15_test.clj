(ns aoc.year2017.day15-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day15 :refer [generator]]))

(deftest generator-test
  (let [generator-a (generator 65 16807 2147483647)
        generator-b (generator 8921 48271 2147483647)]
    (is (= [1092455 1181022009 245556042 1744312007 1352636452]
           (take 5 (generator-a))))
    (is (= [430625591 1233683848 1431495498 137874439 285222916]
           (take 5 (generator-b))))))
