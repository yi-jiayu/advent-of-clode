(ns aoc.year2018.day13-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day13 :refer :all]))

(def example-input "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   \n")
(def input (slurp (clojure.java.io/resource "year2018/day13.txt")))

(deftest remove-carts-from-track-test
  (is (= ["  |  "
          "  |  "
          "  |  "
          "--+--"
          "  |  "
          "  |  "
          "  |  "] (remove-carts-from-track ["  |  "
                                             "  v  "
                                             "  |  "
                                             ">-+-<"
                                             "  |  "
                                             "  ^  "
                                             "  |  "]))))

(deftest make-cart-test
  (is (= (->Cart [1 0] :down 0) (make-cart 1 0 \v))))

(deftest carts-in-row-test
  (is (= [(->Cart [3 2] :down 0) (->Cart [3 4] :right 0)]
         (carts-in-row 3 "--v->--"))))

(deftest parse-input-test
  (is (= [["|"
           "|"
           "|"
           "|"
           "|"
           "|"
           "|"] [(->Cart [1 0] :down 0)
                 (->Cart [5 0] :up 0)]] (parse-input "|\nv\n|\n|\n|\n^\n|\n")))
  (is (= [[" |"
           "-+-"
           " |"
           "-+-"
           " |"
           "-+-"
           " |"]
          [(->Cart [5 0] :right 0)]] (parse-input " |\n-+-\n |\n-+-\n |\n>+-\n | \n"))))

(deftest move-cart-test
  (let [track [" |" "-+-" " |" "-+-" " |" "-+-" " |"]
        cart (->Cart [5 0] :right 0)]
    (is (= (->Cart [5 1] :up 1) (move-cart track cart)))
    (is (= (->Cart [4 1] :up 1) (move-cart track (->Cart [5 1] :up 1))))
    (is (= (->Cart [3 1] :up 2) (move-cart track (->Cart [4 1] :up 1))))
    (is (= (->Cart [2 1] :up 2) (move-cart track (->Cart [3 1] :up 2))))
    (is (= (->Cart [1 1] :right 0) (move-cart track (->Cart [2 1] :up 2))))
    (is (= (->Cart [1 2] :right 0) (move-cart track (->Cart [1 1] :right 0)))))
  (let [track ["/-\\" "| |" "\\-/"]
        cart (->Cart [1 0] :up 0)]
    (is (= (->Cart [0 0] :right 0) (move-cart track cart)))
    (is (= (->Cart [0 1] :right 0) (move-cart track (->Cart [0 0] :right 0))))
    (is (= (->Cart [0 2] :down 0) (move-cart track (->Cart [0 1] :right 0)))))
  (let [track ["/---\\"
               "|   |  /----\\"
               "| /-+--+-\\  |"
               "| | |  | |  |"
               "\\-+-/  \\-+--/"
               "  \\------/"]
        cart (->Cart [0 3] :right 0)]
    (is (= (->Cart [0 4] :down 0) (move-cart track cart)))))

(deftest sort-carts-test
  (is (= [(->Cart [0 0] nil nil)
          (->Cart [0 1] nil nil)
          (->Cart [1 0] nil nil)
          (->Cart [1 1] nil nil)] (sort-carts [(->Cart [1 1] nil nil)
                                               (->Cart [0 1] nil nil)
                                               (->Cart [1 0] nil nil)
                                               (->Cart [0 0] nil nil)]))))

(deftest tick-test
  (let [track ["|" "|" "|" "|" "|" "|" "|"]]
    (is (= [[(->Cart [2 0] :down 0)
             (->Cart [4 0] :up 0)] nil] (tick track [(->Cart [1 0] :down 0)
                                                     (->Cart [5 0] :up 0)])))
    (is (= [nil [3 0]] (tick track [(->Cart [2 0] :down 0)
                                    (->Cart [4 0] :up 0)]))))
  (let [track ["/---\\"
               "|   |  /----\\"
               "| /-+--+-\\  |"
               "| | |  | |  |"
               "\\-+-/  \\-+--/"
               "  \\------/"]
        carts [(->Cart [0 3] :right 0)
               (->Cart [3 9] :down 0)]]
    (is (= [[(->Cart [0 4] :down 0)
             (->Cart [4 9] :right 1)] nil] (tick track carts)))))

(deftest render-carts-on-track-test
  (let [track ["/---\\        " "|   |  /----\\" "| /-+--+-\\  |" "| | |  | |  |" "\\-+-/  \\-+--/" "  \\------/"]
        carts [(->Cart [0 2] :right 0)
               (->Cart [3 9] :down 0)]]
    (is (= "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/"
           (render-carts-on-track track carts)))))

(deftest run-until-collision-test
  (let [track ["|" "|" "|" "|" "|" "|" "|"]
        carts [(->Cart [2 0] :down 0)
               (->Cart [4 0] :up 0)]]
    (is (= [3 0] (run-until-collision track carts))))
  (let [[track carts] (parse-input example-input)]
    (is (= [3 7] (run-until-collision track carts))))
  (let [[track carts] (parse-input input)]
    (is (= [62 117] (run-until-collision track carts)))))

(deftest tick'-test
  (let [[track carts] (parse-input "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/")]
    (is (= #{(->Cart [2 2] :down 0)
             (->Cart [6 2] :up 0)
             (->Cart [6 6] :up 0)} (tick' track carts))))
  (let [track ["--------"]
        carts [(->Cart [0 2] :right 0)
               (->Cart [0 3] :left 0)
               (->Cart [0 4] :right 0)]]
    (is (= #{(->Cart [0 5] :right 0)} (tick' track carts)))))

(deftest run-until-one-cart-left-test
  (let [[track carts] (parse-input "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/")]
    (is (= [4 6] (run-until-one-cart-left track carts))))
  (let [[track carts] (parse-input example-input)
        carts (conj carts (->Cart [2 0] :down 0))]
    (is (= [2 7] (run-until-one-cart-left track carts))))
  (let [[track carts] (parse-input input)]
    (is (= [67 69] (run-until-one-cart-left track carts)))))
