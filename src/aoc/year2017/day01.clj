(ns aoc.year2017.day01)

(defn solve-captcha [captcha]
  (let [captcha (str captcha (first captcha))]
    (let [pairs (partition 2 1 captcha)]
      (apply + (map (comp #(Character/digit % 10) first) (filter #(let [[a b] %] (identical? a b)) pairs))))))
