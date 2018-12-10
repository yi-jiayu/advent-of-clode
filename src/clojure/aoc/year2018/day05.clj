(ns aoc.year2018.day05)

(defn reacts?
  [a b]
  (and (not (nil? a))
       (not (nil? b))
       (or (= 32 (- (int a) (int b)))
           (= 32 (- (int b) (int a))))))

(defn react
  ([polymer]
   (react [] polymer))
  ([head tail]
   (loop [head head
          tail tail]
     (if (empty? tail)
       (apply str head)
       (if (reacts? (peek head) (first tail))
         (recur (pop head) (rest tail))
         (recur (conj head (first tail)) (rest tail)))))))

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
