(ns aoc.year2018.day09-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day09 :refer :all])
  (:import (java.util LinkedList)))

(deftest insert-marble-normal-test
  ;(is (= [[0 1] 1] (insert-marble (new LinkedList [0]) 0 1)))
  (is (= [[0 2 1] 1] (insert-marble-normal (new LinkedList [0 1]) 1 2)))
  (is (= [[0 2 1 3] 3] (insert-marble-normal (new LinkedList [0 2 1]) 1 3)))
  (is (= [[0 4 2 1 3] 1] (insert-marble-normal (new LinkedList [0 2 1 3]) 3 4)))
  (is (= [[0 4 2 5 1 3] 3] (insert-marble-normal (new LinkedList [0 4 2 1 3]) 1 5))))

(deftest insert-marble-special-test
  (let [before [0 16 8 17 4 18 9 19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15]
        before-index 13
        after [0 16 8 17 4 18 19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15]
        after-index 6]
    (is (= [after after-index 9] (insert-marble-special (new LinkedList before) before-index)))))

(deftest update-score-test
  (is (= {4 32} (update-score {} 4 32)))
  (is (= {4 64} (update-score {4 32} 4 32))))

(deftest play-marble-game-test
  (is (= {4 32} (play-marble-game 9 25)))
  (is (= 402398 (apply max (vals (play-marble-game 418 70769))))))
