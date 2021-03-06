(ns aoc.year2017.day01)

(defn solve-captcha-part-1 [captcha]
  (let [captcha (str captcha (first captcha))
        pairs (partition 2 1 captcha)]
    (apply + (map #(Character/digit ^Character (first %) 10)
                  (filter #(apply identical? %) pairs)))))

(defn solve-captcha-part-2 [captcha]
  (let [halfway (/ (count captcha) 2)]
    (apply + (map-indexed
              #(let [index-halfway-around (mod (+ %1 halfway) (count captcha))
                     digit-halfway-around (nth captcha index-halfway-around)]
                 (if (= %2 digit-halfway-around)
                   (Character/digit ^Character %2 10)
                   0))
              captcha))))
