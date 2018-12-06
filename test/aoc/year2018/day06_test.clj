(ns aoc.year2018.day06-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day06 :refer :all]))

(def input (-> "year2018/day06.txt"
                (clojure.java.io/resource)
                (slurp)))

(deftest parse-coord-test
  (is (= [1 1] (parse-coord "1, 1"))))

(deftest parse-coords-test
  (is (= [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]] (parse-coords "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"))))

(deftest get-bounding-box-test
  (is (= [[0 0] [5 5]] (get-bounding-box [[0 0] [5 5]])))
  (is (= [[1 1] [8 9]] (get-bounding-box [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]]))))

(deftest points-in-grid-test
  (is (= [[0 0] [0 1] [1 0] [1 1]] (points-in-grid [0 0] [1 1]))))

(deftest manhattan-distance-test
  (is (= 1 (manhattan-distance [0 0] [1 0])))
  (is (= 4 (manhattan-distance [0 0] [-2 -2]))))

(deftest index-of-nearest-centroid-test
  (is (= 0 (index-of-nearest-centroid [0 0] [[0 0] [2 2]])))
  (is (= 1 (index-of-nearest-centroid [1 2] [[0 0] [2 2]])))
  (is (= -1 (index-of-nearest-centroid [1 1] [[0 0] [2 2]]))))

(deftest cluster-test
  (is (= [#{[0 0] [1 0] [0 1]} #{[1 2] [2 1] [2 2]}] (cluster (points-in-grid [0 0] [2 2]) [[0 0] [2 2]]))))

(deftest touching-boundary?-test
  (is (true? (touching-boundary? [0 0] [2 2] [0 1]))))

(deftest remove-infinite-clusters-test
  (is (= [] (remove-infinite-clusters [0 0] [2 2] [#{[1 0] [0 1]} #{[1 2] [2 1]}]))))

(deftest largest-area-test
  (is (= 17 (largest-area [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])))
  (is (= 3449 (largest-area (parse-coords input)))))
