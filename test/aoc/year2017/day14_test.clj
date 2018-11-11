(ns aoc.year2017.day14-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day14 :refer :all]))

(def example-input "flqrgnkx")
(def input "jxqlasbh")

(deftest used-squares-test
  (is (= 29 (used-squares (disk-hashes example-input 8))))
  (is (= 35 (used-squares (disk-hashes input 8)))))

; FIXME: uncomment and delete corresponding *-slow-test namespace once cloverage supports test selectors
;(deftest ^:slow used-squares-slow-test
;  (is (= 8108 (used-squares (disk-hashes example-input))))
;  (is (= 8140 (used-squares (disk-hashes input)))))

(deftest to-bin-test
  (is (clojure.string/starts-with?
        (clojure.string/replace (to-bin [0xa0 0xc2 0x01 0x70]) " " "0")
        "10100000110000100000000101110000")))

(deftest used?-test
  (let [my-grid (to-grid (disk-hashes example-input 8))]
    (is (used? my-grid 0 0))
    (is (used? my-grid 0 1))
    (is (not (used? my-grid 0 2)))
    (is (used? my-grid [0 0]))
    (is (not (used? my-grid [1 0])))
    (is (not (used? my-grid [2 0])))))

(deftest neighbours-test
  (let [my-grid (to-grid (disk-hashes example-input 8))]
    (is (= #{[0 0] [1 1]} (neighbours my-grid [0 1])))))

(deftest explore-test
  (let [my-grid (to-grid (disk-hashes example-input 8))]
    (is (= #{[2 4] [3 4] [4 4] [5 4] [3 5]}
           (explore my-grid [2 4])))))

(deftest count-regions-test
  (is (= 12 (count-regions (to-grid (disk-hashes example-input 8)))))
  (is (= 4 (count-regions (to-grid (disk-hashes input 8))))))

; FIXME: uncomment and delete corresponding *-slow-test namespace once cloverage supports test selectors
;(deftest ^:slow count-regions-slow-test
;  (is (= 1242 (count-regions (to-grid (disk-hashes example-input)))))
;  (is (= 1182 (count-regions (to-grid (disk-hashes input))))))
