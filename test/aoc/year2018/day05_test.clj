(ns aoc.year2018.day05-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day05 :refer :all]))

(def input (slurp (io/resource "year2018/day05.txt")))

(deftest react-once-test
  (is (= "" (react-once "aA")))
  (is (= "" (react-once "Aa")))
  (is (= "aA" (react-once "abBA")))
  (is (= "abAB" (react-once "abAB")))
  (is (= "aabAAB" (react-once "aabAAB"))))

(deftest react-test
  (is (= "" (react "aA")))
  (is (= "" (react "Aa")))
  (is (= "" (react "abBA")))
  (is (= "abAB" (react "abAB")))
  (is (= "aabAAB" (react "aabAAB")))
  (is (= "dabCBAcaDA" (react "dabAcCaCBAcCcaDA"))))

(deftest inert-length-test
  (is (= 10 (inert-length "dabAcCaCBAcCcaDA")))
  (is (= 9238 (inert-length (clojure.string/trim input)))))
