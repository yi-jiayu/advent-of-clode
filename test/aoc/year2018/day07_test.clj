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

(deftest time-required-for-step-test
  (is (= 61 (time-required-for-step 60 \A)))
  (is (= 1 (time-required-for-step 0 \A))))

(deftest time-required-for-steps-test
  (is (= {\A 61 \B 62 \Z 86} (time-required-for-steps 60 [\A \B \Z])))
  (is (= {\A 1 \B 2 \Z 26} (time-required-for-steps 0 [\A \B \Z]))))

(deftest available?-test
  (is (true? (available? {\A #{}} \A)))
  (is (false? (available? {\A #{\B}} \A))))

(deftest decrement-time-left-test
  (is (= {\A 0 \B 1} (decrement-time-left {\A 1 \B 2} [\A \B]))))

(deftest completed-steps-test
  (is (= #{\A} (completed-steps {\A 0})))
  (is (= #{\A} (completed-steps {\A 0 \B 1}))))

(deftest remove-completed-steps-test
  (is (= {\B 1} (remove-completed-steps {\A 0 \B 1}))))

(deftest tick-test
  (testing "when 1/1 workers and 1 step is available"
    (is (= [#{}
            {\B #{}}
            [\B]
            {\A 0
             \B 2}]
           (tick 1
                 #{}
                 {\A #{} \B #{\A}}
                 [\A \B]
                 {\A 1 \B 2}))))
  (testing "when 2/2 workers and 2 steps are available"
    (is (= [#{}
            {\B #{}}
            [\B]
            {\A 0
             \B 2
             \C 0}]
           (tick 2
                 #{}
                 {\A #{} \B #{\A} \C #{}}
                 [\A \C \B]
                 {\A 1 \B 2 \C 1}))))
  (testing "when 1 step is in progress and 1 worker and 1 step is available"
    (is (= [#{\B}
            {\B #{} \C #{\B}}
            [\B \C]
            {\A 0
             \B 1
             \C 2}]
           (tick 2
                 #{\A}
                 {\A #{} \B #{} \C #{\B}}
                 [\A \B \C]
                 {\A 1
                  \B 2
                  \C 2})))))

(deftest construct-test
  (let [reqs (parse-input example-input)]
    (is (= 15 (construct 2 0 reqs (topo-sort reqs)))))
  (let [reqs (parse-input input)]
    (is (= 980 (construct 5 60 reqs (topo-sort reqs))))))
