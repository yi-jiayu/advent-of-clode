(ns aoc.year2017.day15-slow-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day15 :refer :all]))

(deftest final-count-slow-test
  (let [a (generator-a 65)
        b (generator-b 8921)
        a' (generator-a 703)
        b' (generator-b 516)]
    (is (= 588 (final-count a b 40000000)))
    (is (= 594 (final-count a' b' 40000000)))))
