(ns aoc.year2017.day15-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day15 :refer :all]))

(def input "Generator A starts with 703\nGenerator B starts with 516")

(deftest generator-a-test
  (is (= [1092455 1181022009 245556042 1744312007 1352636452]
         (take 5 (generator-a 65)))))

(deftest generator-b-test
  (is (= [430625591 1233683848 1431495498 137874439 285222916]
         (take 5 (generator-b 8921)))))

(deftest lowest-bits-match?-test
  (is (not (lowest-bits-match? 1092455 430625591)))
  (is (not (lowest-bits-match? 1181022009 1233683848)))
  (is (lowest-bits-match? 245556042 1431495498)))

(deftest judge-test
  (let [a (generator-a 65)
        b (generator-b 8921)
        a' (generator-a' 65)
        b' (generator-b' 8921)]
    (is (= [false false true false false]
           (take 5 (judge a b))))
    (is (= [false false false false false]
           (take 5 (judge a' b'))))))

(deftest final-count-test
  (let [a (generator-a 65)
        b (generator-b 8921)
        a' (generator-a' 65)
        b' (generator-b' 8921)]
    (is (= 1 (final-count a b 5)))
    (is (= 1 (final-count a' b' 1056)))))

(deftest parse-input-test
  (is (= [703 516] (parse-input input))))

(deftest generator-a'-test
  (is (= [1352636452 1992081072 530830436 1980017072 740335192]
         (take 5 (generator-a' 65)))))

(deftest generator-b'-test
  (is (= [1233683848 862516352 1159784568 1616057672 412269392]
         (take 5 (generator-b' 8921)))))
