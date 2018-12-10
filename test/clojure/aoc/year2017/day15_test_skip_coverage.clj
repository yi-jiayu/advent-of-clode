(ns aoc.year2017.day15-test-skip-coverage
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day15 :refer :all]))

(deftest ^:slow final-count-slow-test
  (testing "example initial values"
    (let [a (generator-a 65)
          b (generator-b 8921)
          a' (generator-a' 65)
          b' (generator-b' 8921)]
      (is (= 588 (final-count a b 40000000)))
      (is (= 309 (final-count a' b' 5000000)))))
  (testing "actual initial values"
    (let [a (generator-a 703)
          b (generator-b 516)
          a' (generator-a' 703)
          b' (generator-b' 516)]
      (is (= 594 (final-count a b 40000000)))
      (is (= 328 (final-count a' b' 5000000))))))
