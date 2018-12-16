(ns aoc.year2018.day16-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day16 :refer :all]))

(deftest addr-test
  (is (= [1 2 3 0] (addr [1 2 0 0] 0 1 2))))

(deftest addi-test
  (is (= [1 3 0 0] (addi [1 0 0 0] 0 2 1))))

(deftest mulr-test
  (is (= [2 3 6 0] (mulr [2 3 0 0] 0 1 2))))

(deftest muli-test
  (is (= [2 6 0 0] (muli [2 0 0 0] 0 3 1))))

(deftest banr-test
  (is (= [3 6 2 0] (banr [3 6 0 0] 0 1 2))))

(deftest bani-test
  (is (= [3 2 0 0] (bani [3 0 0 0] 0 6 1))))

(deftest borr-test
  (is (= [3 6 7 0] (borr [3 6 0 0] 0 1 2))))

(deftest bori-test
  (is (= [3 7 0 0] (bori [3 0 0 0] 0 6 1))))

(deftest setr-test
  (is (= [3 3 0 0] (setr [3 0 0 0] 0 nil 1))))

(deftest seti-test
  (is (= [0 3 0 0] (seti [0 0 0 0] 3 nil 1))))

(deftest gtir-test
  (is (= [0 1 1 0] (gtir [0 1 2 0] 2 1 2)))
  (is (= [0 1 0 0] (gtir [0 1 2 0] 1 1 2)))
  (is (= [0 1 0 0] (gtir [0 1 2 0] 0 1 2))))

(deftest gtri-test
  (is (= [2 0 1 0] (gtri [2 0 2 0] 0 1 2)))
  (is (= [2 0 0 0] (gtri [2 0 2 0] 0 2 2)))
  (is (= [2 0 0 0] (gtri [2 0 2 0] 0 3 2))))

(deftest gtrr-test
  (is (= [2 1 1 0] (gtrr [2 1 2 0] 0 1 2)))
  (is (= [2 2 0 0] (gtrr [2 2 2 0] 0 1 2)))
  (is (= [2 3 0 0] (gtrr [2 3 2 0] 0 1 2))))

(deftest eqir-test
  (is (= [0 1 1 0] (eqir [0 1 2 0] 1 1 2)))
  (is (= [0 1 0 0] (eqir [0 1 2 0] 2 1 2))))

(deftest eqri-test
  (is (= [0 1 1 0] (eqri [0 1 2 0] 1 1 2)))
  (is (= [0 1 0 0] (eqri [0 1 2 0] 1 2 2))))

(deftest eqrr-test
  (is (= [1 1 1 0] (eqrr [1 1 2 0] 0 1 2)))
  (is (= [1 2 0 0] (eqrr [1 2 2 0] 0 1 2))))
