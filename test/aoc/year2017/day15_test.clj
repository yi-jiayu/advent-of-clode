(ns aoc.year2017.day15-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day15 :refer :all]))

(def input "Generator A starts with 703\nGenerator B starts with 516")

(deftest generator-test
  (let [a (generator-a 65)
        b (generator-b 8921)]
    (is (= [1092455 1181022009 245556042 1744312007 1352636452]
           (take 5 (a))))
    (is (= [430625591 1233683848 1431495498 137874439 285222916]
           (take 5 (b))))))

(deftest lowest-bits-match?-test
  (is (not (lowest-bits-match? 1092455 430625591)))
  (is (not (lowest-bits-match? 1181022009 1233683848)))
  (is (lowest-bits-match? 245556042 1431495498)))

(deftest judge-test
  (let [a (generator-a 65)
        b (generator-b 8921)]
    (is (= [false false true false false]
           (take 5 (judge a b))))))

(deftest final-count-test
  (let [a (generator-a 65)
        b (generator-b 8921)]
    (is (= 1 (final-count a b 5)))))

(deftest ^:slow final-count-slow-test
  (let [a (generator-a 65)
        b (generator-b 8921)
        a' (generator-a 703)
        b' (generator-b 516)]
    (is (= 588 (final-count a b 40000000)))
    (is (= 594 (final-count a' b' 40000000)))))

(deftest parse-input-test
  (is (= [703 516] (parse-input input))))
