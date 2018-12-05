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

(defn units
  [polymer]
  (into #{} (clojure.string/upper-case polymer)))

(defn remove-unit
  [polymer unit]
  (remove #{unit (char (+ (int unit) 32))} polymer))

(defn inert-length-removing-unit
  [polymer unit]
  (inert-length (remove-unit polymer unit)))

(defn shortest-inert-length-removing-unit
  [polymer]
  (apply min
         (pmap (partial inert-length-removing-unit polymer)
               (units polymer))))
