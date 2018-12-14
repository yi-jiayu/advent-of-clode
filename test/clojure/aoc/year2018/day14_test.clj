(ns aoc.year2018.day14-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day14 :refer :all]))

(deftest combine-recipes-test
  (is (= [1 0] (combine-recipes 3 7)))
  (is (= [5] (combine-recipes 3 2))))

(deftest scoreboard-after-n-recipes-test
  (is (= [3 7] (scoreboard-after-n-recipes 2)))
  (is (= [3 7 1 0] (scoreboard-after-n-recipes 4)))
  (is (= [3 7 1 0 1 0] (scoreboard-after-n-recipes 6)))
  (is (= [3 7 1 0 1 0 1] (scoreboard-after-n-recipes 7))))

(deftest last-m-recipes-after-making-n-test
  (is (= "5158916779" (apply str (last-m-recipes-after-making-n 9 10))))
  (is (= "0124515891" (apply str (last-m-recipes-after-making-n 5 10))))
  (is (= "9251071085" (apply str (last-m-recipes-after-making-n 18 10))))
  (is (= "5941429882" (apply str (last-m-recipes-after-making-n 2018 10)))))

(deftest scoreboard-until-specfic-recipe-sequence-test
  (is (= 9 (scoreboard-until-specfic-recipe-sequence [5 1 5 8 9])))
  (is (= 5 (scoreboard-until-specfic-recipe-sequence [0 1 2 4 5])))
  (is (= 18 (scoreboard-until-specfic-recipe-sequence [9 2 5 1 0])))
  (is (= 2018 (scoreboard-until-specfic-recipe-sequence [5 9 4 1 4]))))
