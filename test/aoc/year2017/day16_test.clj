(ns aoc.year2017.day16-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day16 :refer :all]))

(def example-input "pk/f,x10/12,s13")
(def input (slurp (io/resource "year2017/day16.txt")))

(deftest spin-test
  (is (= (into [] "cdeab") (spin 3 (into [] "abcde")))))

(deftest exchange-test
  (is (= (into [] "eabdc") (exchange 3 4 (into [] "eabcd")))))

(deftest partner-test
  (is (= (into [] "baedc") (partner \e \b (into [] "eabdc")))))

(deftest parse-input-test
  (is (= [[:p \k \f] [:x 10 12] [:s 13]] (parse-input example-input))))

(deftest programs-test
  (is (= (into [] "abcde") (programs 5))))

(deftest dance-test
  (is (= (into [] "baedc") (dance (parse-input "s1,x3/4,pe/b") (into [] "abcde"))))
  (is (= (into [] "padheomkgjfnblic") (dance (parse-input input) (programs 16)))))
