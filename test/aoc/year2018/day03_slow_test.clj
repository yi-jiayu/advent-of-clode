(ns aoc.year2018.day03-slow-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day03 :refer :all]))

(def input (slurp (io/resource "year2018/day03.txt")))

(deftest count-disputed-test
  (is (= 107663 (count-disputed (->> input
                                     (parse-claims)
                                     (mark-claims (make-cloth 1000)))))))

(deftest find-non-overlapping-claim-test
  (is (= {:id 1166, :col-offset 126, :row-offset 200, :width 10, :height 11}
         (find-non-overlapping-claim (parse-claims input)))))
