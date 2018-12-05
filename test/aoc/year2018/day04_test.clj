(ns aoc.year2018.day04-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [aoc.year2018.day04 :refer :all])
  (:import (java.time LocalDateTime)))

(def input (slurp (io/resource "year2018/day04.txt")))

(deftest parse-log-line-test
  (is (= {:timestamp    (LocalDateTime/of 1518 9 16 23 57)
          :guard        1889
          :falls-asleep nil
          :wakes-up     nil}
         (parse-log-line "[1518-09-16 23:57] Guard #1889 begins shift")))
  (is (= {:timestamp    (LocalDateTime/of 1518 5 27 0 47)
          :guard        nil
          :falls-asleep nil
          :wakes-up     "wakes up"}
         (parse-log-line "[1518-05-27 00:47] wakes up")))
  (is (= {:timestamp    (LocalDateTime/of 1518 9 26 0 48)
          :guard        nil
          :falls-asleep "falls asleep"
          :wakes-up     nil}
         (parse-log-line "[1518-09-26 00:48] falls asleep"))))

(deftest parse-logs-test
  (is (= [{:timestamp    (LocalDateTime/of 1518 11 01 0 0)
           :guard        10
           :falls-asleep nil
           :wakes-up     nil}
          {:timestamp    (LocalDateTime/of 1518 11 01 0 5)
           :guard        nil
           :falls-asleep "falls asleep"
           :wakes-up     nil}
          {:timestamp    (LocalDateTime/of 1518 11 01 0 25)
           :guard        nil
           :falls-asleep nil
           :wakes-up     "wakes up"}]
         (parse-logs "[1518-11-01 00:05] falls asleep
                      [1518-11-01 00:00] Guard #10 begins shift
                      [1518-11-01 00:25] wakes up
                      "))))

(deftest minutes-between-test
  (is (= 10 (minutes-between (LocalDateTime/of 2018 12 4 15 25)
                             (LocalDateTime/of 2018 12 4 15 35)))))

(deftest update-total-minutes-slept-test
  (let [guard-id 10
        time-slept (LocalDateTime/of 1518 11 01 0 5)
        time-woke (.plusMinutes time-slept 20)]
    (is (= {10 20} (update-total-minutes-slept {} guard-id time-slept time-woke)))
    (is (= {10 40} (update-total-minutes-slept {10 20} guard-id time-slept time-woke)))))

(deftest calculate-total-minutes-slept-test
  (let [logs (parse-logs "[1518-11-01 00:00] Guard #10 begins shift
                          [1518-11-01 00:05] falls asleep
                          [1518-11-01 00:25] wakes up
                          [1518-11-01 00:30] falls asleep
                          [1518-11-01 00:55] wakes up
                          [1518-11-01 23:58] Guard #99 begins shift
                          [1518-11-02 00:40] falls asleep
                          [1518-11-02 00:50] wakes up
                          [1518-11-03 00:05] Guard #10 begins shift
                          [1518-11-03 00:24] falls asleep
                          [1518-11-03 00:29] wakes up
                          [1518-11-04 00:02] Guard #99 begins shift
                          [1518-11-04 00:36] falls asleep
                          [1518-11-04 00:46] wakes up
                          [1518-11-05 00:03] Guard #99 begins shift
                          [1518-11-05 00:45] falls asleep
                          [1518-11-05 00:55] wakes up")]
    (is (= {10 20} (calculate-total-minutes-slept (take 3 logs))))
    (is (= {10 45} (calculate-total-minutes-slept (take 5 logs))))
    (is (= {10 45 99 10} (calculate-total-minutes-slept (take 8 logs))))
    (is (= {10 50 99 30} (calculate-total-minutes-slept logs)))))

(deftest laziest-guard-test
  (is (= 10 (laziest-guard {10 50 99 30})))
  (is (= 2633 (laziest-guard (-> input
                                 parse-logs
                                 calculate-total-minutes-slept)))))

(deftest update-minutes-asleep-count-test
  (let [time-slept (LocalDateTime/of 1518 11 01 0 5)
        time-woke (.plusMinutes time-slept 20)]
    (is (= (into [] (flatten (map repeat [5 20 35] [0 1 0 1 0])))
           (update-minutes-asleep-count (into [] (repeat 60 0)) time-slept time-woke)))))

(deftest calculate-minutes-asleep-count-test
  (let [logs (parse-logs "[1518-11-01 00:00] Guard #10 begins shift
                          [1518-11-01 00:05] falls asleep
                          [1518-11-01 00:25] wakes up
                          [1518-11-01 00:30] falls asleep
                          [1518-11-01 00:55] wakes up
                          [1518-11-01 23:58] Guard #99 begins shift
                          [1518-11-02 00:40] falls asleep
                          [1518-11-02 00:50] wakes up
                          [1518-11-03 00:05] Guard #10 begins shift
                          [1518-11-03 00:24] falls asleep
                          [1518-11-03 00:29] wakes up
                          [1518-11-04 00:02] Guard #99 begins shift
                          [1518-11-04 00:36] falls asleep
                          [1518-11-04 00:46] wakes up
                          [1518-11-05 00:03] Guard #99 begins shift
                          [1518-11-05 00:45] falls asleep
                          [1518-11-05 00:55] wakes up")
        guard-id 10]
    (is (= 24 (calculate-minutes-asleep-count logs guard-id))))
  (is (= 29 (calculate-minutes-asleep-count (parse-logs input) 2633))))
