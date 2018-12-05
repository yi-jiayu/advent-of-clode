(ns aoc.year2018.day05)

(defn react-once
  ([polymer]
   (react-once [] polymer))
  ([head tail]
   (loop [head head
          tail tail]
     (if (empty? tail)
       (apply str head)
       (if (= 32 (Math/abs ^int (apply - (map int (take 2 tail)))))
         (recur head (drop 2 tail))
         (recur (conj head (first tail)) (rest tail)))))))

(defn react
  [polymer]
  (loop [polymer polymer]
    (let [polymer' (react-once polymer)]
      (if (= (count polymer) (count polymer'))
        polymer
        (recur polymer')))))

(defn inert-length
  [polymer]
  (count (react polymer)))