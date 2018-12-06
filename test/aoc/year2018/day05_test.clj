(ns aoc.year2018.day05-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day05 :refer :all]))

(def input (-> "year2018/day05.txt"
               (clojure.java.io/resource)
               (slurp)
               (clojure.string/trim)))

(deftest reacts?-test
  (is (true? (reacts? \A \a)))
  (is (true? (reacts? \a \A)))
  (is (false? (reacts? \A \B)))
  (is (false? (reacts? \A \b)))
  (is (false? (reacts? \A nil)))
  (is (false? (reacts? nil nil))))

(deftest react-test
  (is (= "" (react "aA")))
  (is (= "" (react "Aa")))
  (is (= "" (react "abBA")))
  (is (= "abAB" (react "abAB")))
  (is (= "aabAAB" (react "aabAAB")))
  (is (= "dabCBAcaDA" (react "dabAcCaCBAcCcaDA"))))

(deftest inert-length-test
  (is (= 10 (inert-length "dabAcCaCBAcCcaDA")))
  (is (= 9238 (inert-length input))))

(deftest units-test
  (is (= #{\A} (units "Aa")))
  (is (= #{\A \B \C \D} (units "dabAcCaCBAcCcaDA"))))

(deftest remove-unit-test
  (let [polymer "dabAcCaCBAcCcaDA"]
    (is (= "dbcCCBcCcD" (apply str (remove-unit polymer \A))))
    (is (= "daAcCaCAcCcaDA" (apply str (remove-unit polymer \B))))
    (is (= "dabAaBAaDA" (apply str (remove-unit polymer \C))))
    (is (= "abAcCaCBAcCcaA" (apply str (remove-unit polymer \D))))))

(deftest inert-length-removing-unit-test
  (let [polymer "dabAcCaCBAcCcaDA"]
    (is (= 6 (inert-length-removing-unit polymer \A)))
    (is (= 8 (inert-length-removing-unit polymer \B)))
    (is (= 4 (inert-length-removing-unit polymer \C)))
    (is (= 6 (inert-length-removing-unit polymer \D)))))

(deftest shortest-inert-length-removing-unit-test
  (let [polymer "dabAcCaCBAcCcaDA"]
    (is (= 4 (shortest-inert-length-removing-unit polymer)))))
