(ns aoc.year2017.day17-test
  (:import (java.util LinkedList))
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day17 :refer :all]))

(def input 345)

(deftest spinlock-insert-test
  (is (= [[0 1] 1] (spinlock-insert 3 (new LinkedList [0]) 0 1)))
  (is (= [[0 2 1] 1] (spinlock-insert 3 (new LinkedList [0 1]) 1 2)))
  (is (= [[0 2 3 1] 2] (spinlock-insert 3 (new LinkedList [0 2 1]) 1 3))))

(deftest spinlock-test
  (is (= [0 9 5 7 2 4 3 8 6 1] (first (spinlock 9 3)))))

(deftest short-circuit-test
  (is (= 638 (short-circuit 2017 3)))
  (is (= 866 (short-circuit 2017 input))))

(deftest track-value-after-zero-test
  (is (= 9 (track-value-after-zero 9 3)))
  (is (= 9 (track-value-after-zero 50e6 input))))
