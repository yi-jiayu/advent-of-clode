(ns aoc.year2018.day13-test
  (:require [clojure.test :refer :all]
            [aoc.year2018.day13 :refer :all]))

(def example-input "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   \n")

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
  (is (= (->Cart [0 1] :down 0) (make-cart 1 0 \v))))

(deftest carts-in-row-test
  (is (= [(->Cart [2 3] :down 0) (->Cart [4 3] :right 0)]
         (carts-in-row 3 "--v->--"))))

(deftest parse-input-test
  (is (= [["|"
           "|"
           "|"
           "|"
           "|"
           "|"
           "|"] [(->Cart [0 1] :down 0)
                 (->Cart [0 5] :up 0)]] (parse-input "|\nv\n|\n|\n|\n^\n|\n")))
  (is (= [[" |"
           "-+-"
           " |"
           "-+-"
           " |"
           "-+-"
           " |"]
          [(->Cart [0 5] :right 0)]] (parse-input " |\n-+-\n |\n-+-\n |\n>+-\n | \n"))))

(deftest move-cart-test
  (let [track [" |" "-+-" " |" "-+-" " |" "-+-" " |"]
        cart (->Cart [0 5] :right 0)]
    (is (= (->Cart [1 5] :up 1) (move-cart track cart)))
    (is (= (->Cart [1 4] :up 1) (move-cart track (->Cart [1 5] :up 1))))
    (is (= (->Cart [1 3] :up 2) (move-cart track (->Cart [1 4] :up 1))))
    (is (= (->Cart [1 2] :up 2) (move-cart track (->Cart [1 3] :up 2))))
    (is (= (->Cart [1 1] :right 0) (move-cart track (->Cart [1 2] :up 2))))
    (is (= (->Cart [2 1] :right 0) (move-cart track (->Cart [1 1] :right 0)))))
  (let [track ["/-\\" "| |" "\\-/"]
        cart (->Cart [0 1] :up 0)]
    (is (= (->Cart [0 0] :right 0) (move-cart track cart)))
    (is (= (->Cart [1 0] :right 0) (move-cart track (->Cart [0 0] :right 0))))
    (is (= (->Cart [2 0] :down 0) (move-cart track (->Cart [1 0] :right 0)))))
  (let [track ["/---\\"
               "|   |  /----\\"
               "| /-+--+-\\  |"
               "| | |  | |  |"
               "\\-+-/  \\-+--/"
               "  \\------/"]
        cart (->Cart [3 0] :right 0)]
    (is (= (->Cart [4 0] :down 0) (move-cart track cart)))))

(deftest sort-carts-test
  (is (= [(->Cart [0 0] nil nil)
          (->Cart [1 0] nil nil)
          (->Cart [0 1] nil nil)
          (->Cart [1 1] nil nil)] (sort-carts [(->Cart [0 0] nil nil)
                                               (->Cart [1 1] nil nil)
                                               (->Cart [0 1] nil nil)
                                               (->Cart [1 0] nil nil)]))))

(deftest tick-test
  (let [track ["|" "|" "|" "|" "|" "|" "|"]]
    (is (= [[(->Cart [0 2] :down 0)
             (->Cart [0 4] :up 0)] nil] (tick track [(->Cart [0 1] :down 0)
                                                     (->Cart [0 5] :up 0)])))
    (is (= [[(->Cart [0 3] :down 0)
             (->Cart [0 3] :up 0)] [0 3]] (tick track [(->Cart [0 2] :down 0)
                                                       (->Cart [0 4] :up 0)]))))
  (let [track ["/---\\"
               "|   |  /----\\"
               "| /-+--+-\\  |"
               "| | |  | |  |"
               "\\-+-/  \\-+--/"
               "  \\------/"]
        carts [(->Cart [3 0] :right 0)
               (->Cart [9 3] :down 0)]]
    (is (= [[(->Cart [4 0] :down 0)
             (->Cart [9 4] :right 1)] nil] (tick track carts)))))

(deftest render-carts-on-track-test
  (let [track ["/---\\        " "|   |  /----\\" "| /-+--+-\\  |" "| | |  | |  |" "\\-+-/  \\-+--/" "  \\------/"]
        carts [#aoc.year2018.day13.Cart{:position [2 0], :direction :right, :state 0} #aoc.year2018.day13.Cart{:position [9 3], :direction :down, :state 0}]]
    (is (= "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/"
           (render-carts-on-track track carts)))))

(deftest run-until-collision-test
  (let [track ["|" "|" "|" "|" "|" "|" "|"]
        carts [(->Cart [0 2] :down 0)
               (->Cart [0 4] :up 0)]]
    (is (= [0 3] (run-until-collision track carts))))
  (let [[track carts] (parse-input example-input)]
    (is (= [7 3] (run-until-collision track carts)))
    ))
