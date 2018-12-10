(ns aoc.year2018.day03-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day03 :refer :all]))

(deftest parse-claim-test
  (is (= {:id 15, :col-offset 110, :row-offset 919, :width 19, :height 18}
         (parse-claim "#15 @ 110,919: 19x18"))))

(deftest parse-claims-test
  (is (= [{:id 19, :col-offset 727, :row-offset 848, :width 19, :height 18}
          {:id 20, :col-offset 165, :row-offset 463, :width 10, :height 18}
          {:id 21, :col-offset 108, :row-offset 559, :width 21, :height 23}]
         (parse-claims "#19 @ 727,848: 19x18
         #20 @ 165,463: 10x18
         #21 @ 108,559: 21x23"))))

(deftest should-update?-test
  (is (true? (should-update? 0 2 0)))
  (is (true? (should-update? 2 2 3)))
  (is (false? (should-update? 3 5 2)))
  (is (true? (should-update? 3 5 3)))
  (is (true? (should-update? 3 5 7)))
  (is (false? (should-update? 3 5 8))))

(deftest should-update-column?-test
  (is (false? (should-update-column? {:col-offset 3 :width 5} 2)))
  (is (true? (should-update-column? {:col-offset 3 :width 5} 3))))

(deftest should-update-row?-test
  (is (false? (should-update-row? {:row-offset 3 :height 5} 2)))
  (is (true? (should-update-row? {:row-offset 3 :height 5} 3))))

(deftest update-column-test
  (let [claim {:col-offset 1 :width 2}]
    (is (= 0 (update-column claim 0 0)))
    (is (= 1 (update-column claim 1 0)))
    (is (= 2 (update-column claim 1 1)))))

(deftest update-row-test
  (let [claim {:row-offset 1 :col-offset 1 :width 2 :height 2}]
    (is (= [0 0 0 0] (update-row claim 0 [0 0 0 0])))
    (is (= [0 1 1 0] (update-row claim 1 [0 0 0 0])))
    (is (= [1 2 1 0] (update-row claim 1 [1 1 0 0])))))

(deftest mark-claim-test
  (let [cloth (make-cloth 4)]
    (is (= [[0 0 0 0]
            [0 1 1 0]
            [0 1 1 0]
            [0 0 0 0]] (mark-claim cloth {:row-offset 1 :col-offset 1 :width 2 :height 2})))
    (is (= [[1 1 0 0]
            [1 1 0 0]
            [0 0 0 0]
            [0 0 0 0]] (mark-claim cloth {:row-offset 0 :col-offset 0 :width 2 :height 2})))
    (is (= [[0 0 0 0]
            [0 0 0 0]
            [0 0 1 1]
            [0 0 1 1]] (mark-claim cloth {:row-offset 2 :col-offset 2 :width 2 :height 2})))))

(deftest mark-claims-test
  (let [cloth (make-cloth 4)
        claims [{:row-offset 1 :col-offset 1 :width 2 :height 2}
                {:row-offset 0 :col-offset 0 :width 2 :height 2}
                {:row-offset 2 :col-offset 2 :width 2 :height 2}]]
    (is (= [[1 1 0 0]
            [1 2 1 0]
            [0 1 2 1]
            [0 0 1 1]] (mark-claims cloth claims)))))

(deftest count-disputed-test
  (is (= 2 (count-disputed [[1 1 0 0]
                            [1 2 1 0]
                            [0 1 2 1]
                            [0 0 1 1]])))
  (is (= 4 (count-disputed [[0 0 0 0 0 0 0 0]
                            [0 0 0 1 1 1 1 0]
                            [0 0 0 1 1 1 1 0]
                            [0 1 1 2 2 1 1 0]
                            [0 1 1 2 2 1 1 0]
                            [0 1 1 1 1 1 1 0]
                            [0 1 1 1 1 1 1 0]
                            [0 0 0 0 0 0 0 0]]))))

(let [claim1 (parse-claim "#1 @ 1,3: 4x4")
      claim2 (parse-claim "#2 @ 3,1: 4x4")
      claim3 (parse-claim "#3 @ 5,5: 2x2")
      claim123 (parse-claim "#123 @ 3,2: 5x4")]

  (deftest edges-test
    (is (= [1 2 1 2] (edges {:col-offset 1 :row-offset 1 :width 2 :height 2})))
    (is (= [3 6 1 4] (edges claim1)))
    (is (= [1 4 3 6] (edges claim2)))
    (is (= [5 6 5 6] (edges claim3)))
    (is (= [2 5 3 7] (edges claim123))))

  (deftest intersects?-test
    (testing "should not intersect if id is the same"
      (is (false? (intersects? claim1 claim1))))
    (testing "should not intersect when only overlapping by one unit"
      (is (true? (intersects? {:id 1 :col-offset 1 :row-offset 1 :width 2 :height 2}
                              {:id 2 :col-offset 2 :row-offset 2 :width 2 :height 2}))))
    (is (true? (intersects? claim1 claim2)))
    (is (false? (intersects? claim1 claim3)))
    (is (false? (intersects? claim2 claim3))))

  (let [claims [claim1 claim2 claim3]]

    (deftest intersects-none?-test
      (is (false? (intersects-none? claims claim1)))
      (is (false? (intersects-none? claims claim2)))
      (is (true? (intersects-none? claims claim3))))

    (deftest find-non-overlapping-claim-test
      (is (= claim3 (find-non-overlapping-claim claims))))))
