(ns aoc.year2017.day16-test
  (:require [clojure.test :refer :all])
  (:require [aoc.year2017.day16 :refer :all]))

(deftest spin-test
  (is (= (into [] "cdeab") (spin 3 (into [] "abcde")))))

(deftest exchange-test
  (is (= (into [] "eabdc") (exchange 3 4 (into [] "eabcd")))))

(deftest partner-test
  (is (= (into [] "baedc") (partner \e \b (into [] "eabdc")))))
