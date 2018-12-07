(ns aoc.year2018.day07-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day07 :refer :all]))

(def example-input "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n")
(def input (slurp (clojure.java.io/resource "year2018/day07.txt")))

(deftest parse-line-test
  (is (= [\U \R] (parse-line "Step U must be finished before step R can begin."))))

(deftest parse-input-test
  (is (= [[\U \R] [\C \B] [\A \S]] (parse-input "Step U must be finished before step R can begin.\nStep C must be finished before step B can begin.\nStep A must be finished before step S can begin.\n"))))

(deftest all-steps-test
  (is (= #{\A \B \C \D \E \F} (all-steps (parse-input example-input)))))

(deftest update-dependency-list-test
  (is (= {\A #{} \B #{\A}}
         (update-dependency-list {\A #{} \B #{}}
                                 [\A \B])))
  (is (= {\A #{\C} \B #{\A} \C {}}
         (update-dependency-list {\A #{} \B #{\A} \C {}}
                                 [\C \A])))
  (is (= {\A #{\C} \B #{\A \D} \C #{} \D #{}}
         (update-dependency-list {\A #{\C} \B #{\A} \C #{} \D #{}}
                                 [\D \B]))))

(deftest build-dependency-list-test
  (is (= {\A #{\C}
          \B #{\A}
          \C #{}
          \D #{\A}
          \F #{\C}
          \E #{\B \D \F}}
         (build-dependency-list (parse-input example-input)))))

(deftest available-steps-test
  (is (= #{\C} (available-steps (-> example-input
                                    (parse-input)
                                    (build-dependency-list))))))

(deftest remove-alphabetical-first-test
  (is (= [\A #{\B \C}] (remove-alphabetical-first #{\A \B \C}))))

(deftest remove-dependency-test
  (is (= {\A #{}} (remove-dependency {\A #{\B}} \B)))
  (is (= {\A #{} \B #{}} (remove-dependency {\A #{\C} \B #{\C}} \C)))
  (is (= {\A #{} \B #{\D}} (remove-dependency {\A #{\C} \B #{\C \D}} \C))))

(deftest topo-sort-test
  (is (= "CABDFE" (apply str (topo-sort (parse-input example-input)))))
  (is (= "ACBDESULXKYZIMNTFGWJVPOHRQ" (apply str (topo-sort (parse-input input))))))
