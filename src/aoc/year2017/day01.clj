(ns aoc.year2017.day01)

(defn solve-captcha [captcha]
  (let [captcha (str captcha (first captcha))]
    (let [pairs (partition 2 1 captcha)]
      (apply + (map #(Character/digit (first %) 10)
                    (filter #(apply identical? %) pairs))))))
