(ns aoc.year2017.day01)

(defn solve-captcha [captcha]
  (let [captcha (str captcha (first captcha))]
    (loop [sum 0
           prev nil
           captcha captcha]
      (let [curr (first captcha)]
        (if (empty? captcha) sum
            (recur (if (= curr prev)
                     (+ sum (Character/digit prev 10))
                     sum)
                   curr
                   (rest captcha)))))))
