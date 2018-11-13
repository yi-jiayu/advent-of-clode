(ns aoc.year2017.day18-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [aoc.year2017.day18 :refer :all]))

(def example-input "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2")
(def example-instructions [[:set :a 1] [:add :a 2] [:mul :a :a] [:mod :a 5] [:snd :a] [:set :a 0] [:rcv :a] [:jgz :a -1] [:set :a 1] [:jgz :a -2]])
(def input (slurp (io/resource "year2017/day18.txt")))

(deftest set-register-test
  (is (= ({:a 1} (set-register :a 1 {})))))

(deftest update-register-test
  (is (= ({:a 1} (update-register :a inc {}))))
  (is (= ({:a 6} (update-register :a (partial * 2) {:a 3})))))

(deftest get-register-test
  (let [registers {:a 1}]
    (is (= 0 (get-register :b registers)))
    (is (= 1 (get-register :a registers)))))

(deftest inc-pc-test
  (is (= {:pc 1} (inc-pc {})))
  (is (= {:pc 2} (inc-pc {:pc 1}))))

(deftest value-of-test
  (is (= 1 (value-of 1 {})))
  (is (= 1 (value-of :a {:a 1}))))

(deftest isound-test
  (is (= {:pc 1 :output 1} (isound {} 1)))
  (is (= {:a 1 :pc 1 :output 1} (isound {:a 1} :a))))

(deftest iset-test
  (is (= {:a 1 :pc 1} (iset {} :a 1)))
  (is (= {:a 2 :b 2 :pc 1} (iset {:b 2} :a :b))))

(deftest iadd-test
  (is (= {:a 3 :pc 1} (iadd {:a 1} :a 2)))
  (is (= {:a 6 :pc 1} (iadd {:a 3} :a :a))))

(deftest imul-test
  (is (= {:a 6 :pc 1} (imul {:a 2} :a 3)))
  (is (= {:a 9 :pc 1} (imul {:a 3} :a :a))))

(deftest imod-test
  (is (= {:a 2 :pc 1} (imod {:a 5} :a 3)))
  (is (= {:a 2 :b 3 :pc 1} (imod {:a 5 :b 3} :a :b))))

(deftest irecover-test
  (is (= {:pc 2 :output 1} (irecover {:pc 1 :output 1} 0)))
  (is (= {:pc 2 :output 1 :recovered 1} (irecover {:pc 1 :output 1} 1)))
  (is (= {:a 0 :pc 2 :output 1} (irecover {:a 0 :pc 1 :output 1} :a)))
  (is (= {:a 1 :pc 2 :output 1 :recovered 1} (irecover {:a 1 :pc 1 :output 1} :a))))

(deftest ijgz-test
  (is (= {:pc 1} (ijgz {} 0 5)))
  (is (= {:pc 5} (ijgz {} 1 5)))
  (is (= {:pc 2} (ijgz {:pc 5} 1 -3)))
  (is (= {:a 0 :pc 1} (ijgz {:a 0} :a 5)))
  (is (= {:a 1 :pc 5} (ijgz {:a 1} :a 5)))
  (is (= {:a 1 :pc 2} (ijgz {:a 1 :pc 5} :a -3))))

(deftest integer-or-keyword-test
  (is (= 1 (integer-or-keyword "1")))
  (is (= -1 (integer-or-keyword "-1")))
  (is (= :a (integer-or-keyword "a"))))

(deftest parse-instruction-test
  (is (= [:set :a 1] (parse-instruction "set a 1")))
  (is (= [:mul :a :a] (parse-instruction "mul a a"))))

(deftest parse-instructions-test
  (is (= example-instructions (parse-instructions example-input))))

(deftest soundcard-execute-test
  (is (= {:pc 4 :a 4}
         (soundcard-execute example-instructions #(= 4 (get-register :pc %)))))
  (is (= {:pc 5 :a 4 :output 4}
         (soundcard-execute example-instructions #(= 5 (get-register :pc %)))))
  (is (= {:pc 7 :a 1 :output 4 :recovered 4}
         (soundcard-execute example-instructions #(not= 0 (get-register :recovered %)))))
  (is (= 3423
         (:recovered (soundcard-execute (parse-instructions input) #(not= 0 (get-register :recovered %)))))))

(deftest isend-test
  (let [my-chan (async/chan 1)]
    (testing "should successfully send value to channel"
      (is (= 1
             (do (isend {:out my-chan} 1)
                 (first (async/alts!! [my-chan (async/timeout 100)]))))))))

(deftest ireceive-test
  (testing "should successfully receive value from channel"
    (let [my-chan (async/chan 1)]
      (is (= 1
             (do (async/>!! my-chan 1)
                 (get-register :a (ireceive {:in my-chan} :a))))))))
