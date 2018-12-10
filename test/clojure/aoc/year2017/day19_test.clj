(ns aoc.year2017.day19-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day19 :refer :all]
            [clojure.java.io :as io]))

(def example-input "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ \n")
(def example-diagram ["     |          "
                      "     |  +--+    "
                      "     A  |  C    "
                      " F---|----E|--+ "
                      "     |  |  |  D "
                      "     +B-+  +--+ "])
(def input (slurp (io/resource "year2017/day19.txt")))

(deftest load-routing-diagram-test
  (is (= example-diagram
         (load-routing-diagram example-input))))

(deftest find-start-test
  (is (= [0 5] (find-start example-diagram))))

(deftest path-forward?-test
  (testing "out of bounds"
    (is (false? (path-forward? example-diagram [0 0] :up))))
  (testing "no path"
    (is (false? (path-forward? example-diagram [0 5] :left))))
  (testing "valid path"
    (is (true? (path-forward? example-diagram [0 5] :down)))))

(deftest next-step-test
  (testing "next step from starting position"
    (is (= [[1 5] :down] (next-step example-diagram [0 5] :down))))
  (testing "next step from first crossroads"
    (is (= [[4 5] :down] (next-step example-diagram [3 5] :down))))
  (testing "next step from first turn"
    (is (= [[5 6] :right] (next-step example-diagram [5 5] :down)))))

(deftest follow-test
  (is (= ["ABCDEF" 38] (follow example-diagram)))
  (is (= ["SXWAIBUZY" 16676] (follow (load-routing-diagram input)))))
