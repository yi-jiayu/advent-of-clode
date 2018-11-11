(ns aoc.year2017.day14-slow-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day14 :refer :all]))

(def example-input "flqrgnkx")
(def input "jxqlasbh")

(deftest used-squares-slow-test
  (is (= 8108 (used-squares (disk-hashes example-input))))
  (is (= 8140 (used-squares (disk-hashes input)))))

(deftest count-regions-slow-test
  (is (= 1242 (count-regions (to-grid (disk-hashes example-input)))))
  (is (= 1182 (count-regions (to-grid (disk-hashes input))))))
