(ns aoc.year2018.day09-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day09 :refer :all])
  (:import (aoc.year2018.day09 Node)))

(deftest insert-marble-normal-test
  (is (= [0 1] (.toList (insert-marble-normal (new Node 0) 1))))
  (is (= [0 2 1] (.toList (-> (new Node 0)
                              (insert-marble-normal 1)
                              (insert-marble-normal 2))))))

(deftest update-score-test
  (is (= {4 32} (update-score {} 4 32)))
  (is (= {4 64} (update-score {4 32} 4 32))))

(deftest play-marble-game-test
  (is (= {4 32} (play-marble-game 9 25)))
  (is (= 402398 (apply max (vals (play-marble-game 418 70769))))))
