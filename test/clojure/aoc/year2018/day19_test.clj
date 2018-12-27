(ns aoc.year2018.day19-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day19 :refer :all]))

(def example-input "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5\n")
(def input (slurp (clojure.java.io/resource "year2018/day19.txt")))

(deftest parse-instruction-test
  (is (= [:seti 5 0 1] (parse-instruction "seti 5 0 1"))))

(deftest parse-instructions-test
  (is (= [[:seti 5 0 1]
          [:seti 6 0 2]
          [:addi 0 1 0]
          [:addr 1 2 3]
          [:setr 1 0 0]
          [:seti 8 0 4]
          [:seti 9 0 5]] (parse-instructions ["seti 5 0 1"
                                              "seti 6 0 2"
                                              "addi 0 1 0"
                                              "addr 1 2 3"
                                              "setr 1 0 0"
                                              "seti 8 0 4"
                                              "seti 9 0 5"]))))

(deftest parse-declaration-test
  (is (= 0 (parse-declaration "#ip 0"))))

(deftest parse-input-test
  (is (= {:ip-binding 0
          :program    [[:seti 5 0 1]
                       [:seti 6 0 2]
                       [:addi 0 1 0]
                       [:addr 1 2 3]
                       [:setr 1 0 0]
                       [:seti 8 0 4]
                       [:seti 9 0 5]]}
         (parse-input example-input))))


(deftest execute-instruction-test
  (is (= {:ip-binding 0
          :ip         1
          :registers  [0 5 0 0 0 0]}
         (execute-instruction {:ip-binding 0
                               :ip         0
                               :registers  [0 0 0 0 0 0]} [:seti 5 0 1])))
  (is (= {:ip-binding 0
          :ip         2
          :registers  [1 5 6 0 0 0]}
         (execute-instruction {:ip-binding 0
                               :ip         1
                               :registers  [0 5 0 0 0 0]} [:seti 6 0 2])))
  (is (= {:ip-binding 0
          :ip         4
          :registers  [3 5 6 0 0 0]}
         (execute-instruction {:ip-binding 0
                               :ip         2
                               :registers  [1 5 6 0 0 0]} [:addi 0 1 0])))
  (is (= {:ip-binding 0
          :ip         6
          :registers  [5 5 6 0 0 0]}
         (execute-instruction {:ip-binding 0
                               :ip         4
                               :registers  [3 5 6 0 0 0]} [:setr 1 0 0])))
  (is (= {:ip-binding 0
          :ip         7
          :registers  [6 5 6 0 0 9]}
         (execute-instruction {:ip-binding 0
                               :ip         6
                               :registers  [5 5 6 0 0 0]} [:seti 9 0 5]))))

(deftest run-program-test
  (is (= {:ip-binding 0
          :ip         7
          :registers  [6 5 6 0 0 9]} (run-program (parse-input example-input))))
  (is (= {:ip         257
          :ip-binding 4
          :registers  [912 911 912 1 256 912]} (run-program (parse-input input)))))
