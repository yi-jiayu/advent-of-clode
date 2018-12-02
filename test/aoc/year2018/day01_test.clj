(ns aoc.year2018.day01-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day01 :refer :all]))

(def example-input "+1\n-2\n+3\n+1\n")
(def example-frequency-changes (parse-frequency-changes example-input))
(def input (slurp (io/resource "year2018/day01.txt")))

(deftest parse-frequency-changes-test
  (is (= [1 -2 3 1] example-frequency-changes)))

(deftest resulting-frequency-test
  (is (= 3 (resulting-frequency [1 -2 3 1])))
  (is (= 411 (resulting-frequency (parse-frequency-changes input)))))


(deftest lazy-frequencies-test
  (is (= [0 1 -1 2 3 4 2] (take 7 (lazy-frequencies example-frequency-changes)))))

(deftest first-frequency-reached-twice-test
  (is (= 2 (first-frequency-reached-twice example-frequency-changes)))
  (is (= 0 (first-frequency-reached-twice [1 -1])))
  (is (= 10 (first-frequency-reached-twice [3 3 4 -2 -4])))
  (is (= 5 (first-frequency-reached-twice [-6 3 8 5 -6])))
  (is (= 14 (first-frequency-reached-twice [7 7 -2 -7 -4])))
  (is (= 14 (first-frequency-reached-twice (parse-frequency-changes input)))))
