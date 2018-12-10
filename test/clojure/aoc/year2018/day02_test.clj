(ns aoc.year2018.day02-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day02 :refer :all]))

(def example-input "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab\n")
(def example-box-ids (parse-box-ids example-input))
(def input (slurp (io/resource "year2018/day02.txt")))

(deftest parse-box-ids-test
  (is (= ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]
         example-box-ids)))

(deftest contains-exactly-n-of-any-char?-test
  (is (contains-exactly-n-of-any-char? 2 "bababc"))
  (is (contains-exactly-n-of-any-char? 3 "bababc")))

(deftest checksum-test
  (is (= 12 (checksum example-box-ids)))
  (is (= 6723 (checksum (parse-box-ids input)))))

(deftest hamming-distance-test
  (is (= 2 (hamming-distance "abcde" "axcye")))
  (is (= 1 (hamming-distance "fghij" "fguij"))))

(deftest common-chars-test
  (is (= "fgij" (common-chars "fghij" "fguij"))))

(deftest correct-box-ids-test
  (is (= ["fghij" "fguij"] (correct-box-ids ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]))))

(deftest common-chars-between-correct-boxes-test
  (is (= "fgij" (common-chars-between-correct-boxes ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])))
  (is (= "prtkqyluiusocwvaezjmhmfgx" (common-chars-between-correct-boxes (parse-box-ids input)))))
