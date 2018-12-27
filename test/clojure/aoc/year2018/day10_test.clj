(ns aoc.year2018.day10-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day10 :refer :all]))

(def example-input "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>
")
(def input (-> "year2018/day10.txt"
               (clojure.java.io/resource)
               (slurp)))

(deftest parse-particle-position-and-velocity-test
  (is (= [9 1 0 2] (parse-particle-position-and-velocity "position=< 9,  1> velocity=< 0,  2>")))
  (is (= [3 -2 -1 1] (parse-particle-position-and-velocity "position=< 3, -2> velocity=<-1,  1>"))))

(deftest load-particles-test
  (let [input "position=< 9,  1> velocity=< 0,  2>
  position=< 7,  0> velocity=<-1,  0>
  position=< 3, -2> velocity=<-1,  1>
  "]
    (is (= [[9 1 0 2]
            [7 0 -1 0]
            [3 -2 -1 1]] (load-particles input)))))

(deftest update-particle-position-test
  (is (= [4 7 1 -2] (update-particle-position [3 9 1 -2])))
  (is (= [5 5 1 -2] (update-particle-position [4 7 1 -2])))
  (is (= [6 3 1 -2] (update-particle-position [5 5 1 -2]))))

(deftest update-particle-positions-test
  (is (= [[9 3 0 2]
          [6 0 -1 0]
          [2 -1 -1 1]] (update-particle-positions [[9 1 0 2]
                                                   [7 0 -1 0]
                                                   [3 -2 -1 1]]))))
(deftest get-bounding-box-test
  (is (= [9 2 3 -1] (get-bounding-box [[9 3 0 2]
                                       [6 0 -1 0]
                                       [2 -1 -1 1]]))))

(deftest bounding-box-size-test
  (is (= 11 (bounding-box-size [[9 3 0 2]
                                [6 0 -1 0]
                                [2 -1 -1 1]]))))

(deftest plot-particle-test
  (is (= [[1 0] [0 0]] (plot-particle [[0 0] [0 0]] 0 0 [0 0 0 0])))
  (is (= [[1 0] [0 0]] (plot-particle [[0 0] [0 0]] 5 -2 [5 -2 0 0]))))

(deftest plot-particles-test
  (is (= [[1 0] [0 1]] (plot-particles [[0 0 0 0]
                                        [1 1 0 0]])))
  (is (= [[1 0] [0 1]] (plot-particles [[6 6 0 0]
                                        [7 7 0 0]])))
  (is (= [[1 0] [0 1]] (plot-particles [[-2 -2 0 0]
                                        [-1 -1 0 0]]))))

(deftest render-particle-plot-test
  (is (= "# \n #" (render-particle-plot [[1 0] [0 1]]))))
(deftest fast-forward-test
  (let [[t particles] (fast-forward (load-particles example-input))]
    (is (= 3 t))
    (is (= "#   #  ###
    #   #   #
    #   #   #
    #####   #
    #   #   #
    #   #   #
    #   #   #
    #   #  ###"
           (-> particles
               (plot-particles)
               (render-particle-plot)))))
  (let [[t particles] (fast-forward (load-particles input))]
    (is (= 10558 t))
    (is (= "#    #  #####   #    #  ######  ######  #    #   ####      ###
    ##   #  #    #  #    #  #            #  #    #  #    #      #
    ##   #  #    #  #    #  #            #  #    #  #           #
    # #  #  #    #  #    #  #           #   #    #  #           #
    # #  #  #####   ######  #####      #    ######  #           #
    #  # #  #    #  #    #  #         #     #    #  #           #
    #  # #  #    #  #    #  #        #      #    #  #           #
    #   ##  #    #  #    #  #       #       #    #  #       #   #
    #   ##  #    #  #    #  #       #       #    #  #    #  #   #
    #    #  #####   #    #  ######  ######  #    #   ####    ###  "
           (-> particles
               (plot-particles)
               (render-particle-plot))))))
