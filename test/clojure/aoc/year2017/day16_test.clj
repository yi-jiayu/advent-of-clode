(ns aoc.year2017.day16-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2017.day16 :refer :all]))

(def example-input "s1,x3/4,pe/b")
(def input (slurp (io/resource "year2017/day16.txt")))

(deftest spin-test
  (is (= (into [] "cdeab") (spin 3 (into [] "abcde")))))

(deftest exchange-test
  (is (= (into [] "eabdc") (exchange 3 4 (into [] "eabcd")))))

(deftest partner-test
  (is (= (into [] "baedc") (partner \e \b (into [] "eabdc")))))

(deftest parse-input-test
  (is (= [[:p \k \f] [:x 10 12] [:s 13]] (parse-input "pk/f,x10/12,s13"))))

(deftest programs-test
  (is (= (into [] "abcde") (programs 5))))

(deftest dance-test
  (is (= (into [] "baedc") (dance (parse-input example-input) (into [] "abcde"))))
  (is (= (into [] "padheomkgjfnblic") (dance (parse-input input) (programs 16)))))

(deftest new-positions-test
  (is (= [1 0 4 3 2] (new-positions (into [] "baedc") (into [] "abcde")))))

(deftest offsets-test
  (is (= [1 -1 2 0 -2] (offsets (into [] "abcde") (into [] "baedc")))))

(deftest apply-new-positions-test
  (is (= (into [] "baedc") (apply-new-positions [1 0 4 3 2] (into [] "abcde")))))

(deftest dance-over-and-over-test
  (let [programs (programs 5)]
    (is (= (into [] "baedc") (dance-over-and-over 1 (parse-input example-input) programs)))
    (is (= (into [] "ceadb") (dance-over-and-over 2 (parse-input example-input) programs)))))
