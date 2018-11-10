(ns aoc.year2017.day10-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day10 :refer :all]))

(def input "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63")

(deftest test-reverse-sublist
  (is (= [2 1 0 3 4] (reverse-sublist [0 1 2 3 4] 0 3)))
  (is (= [4 3 0 1 2] (reverse-sublist [2 1 0 3 4] 3 4)))
  (is (= [4 3 0 1 2] (reverse-sublist [4 3 0 1 2] 1 1)))
  (is (= [4 3 0 1 2] (reverse-sublist [3 4 2 1 0] 1 5))))

(deftest test-knot-hash-update
  (is (= [[2 1 0 3 4] 3 1] (knot-hash-update [[0 1 2 3 4] 0 0] 3)))
  (is (= [[4 3 0 1 2] 3 2] (knot-hash-update [[2 1 0 3 4] 3 1] 4)))
  (is (= [[4 3 0 1 2] 1 3] (knot-hash-update [[4 3 0 1 2] 3 2] 1)))
  (is (= [[3 4 2 1 0] 4 4] (knot-hash-update [[4 3 0 1 2] 1 3] 5))))

(deftest test-knot-hash-round
  (is (= 12 (apply * (take 2 (first (knot-hash-round
                                     [3 4 1 5]
                                     (into [] (take 5 (range)))
                                     0
                                     0))))))
  (is (= 40132 (apply * (take 2 (first (knot-hash-round
                                        (parse-input input)
                                        (into [] (take 256 (range)))
                                        0
                                        0)))))))

(deftest test-parse-input
  (is (= [3 4 1 5] (parse-input "3,4,1,5"))))

(deftest test-parse-input-bytes
  (is (= [49 44 50 44 51] (parse-input-bytes "1,2,3"))))

(deftest test-condense-hash
  (is (= [64]
         (condense-hash [65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22])))
  (is (= [64 64]
         (condense-hash [65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22
                         65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22]))))

(deftest test-to-hex
  (is (= "4007ff" (to-hex [64 7 255]))))

(deftest test-knot-hash
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (to-hex (knot-hash ""))))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (to-hex (knot-hash "AoC 2017"))))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (to-hex (knot-hash "1,2,3"))))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (to-hex (knot-hash "1,2,4"))))
  (is (= "35b028fe2c958793f7d5a61d07a008c8" (to-hex (knot-hash input)))))
