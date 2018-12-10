(ns aoc.year2018.day08)

(defn parse-tree
  [input]
  (as-> input input
        (clojure.string/trim input)
        (clojure.string/split input #" ")
        (mapv #(Integer/parseInt %) input)))

(defn sum-metadata
  "Sums the metadata of `node` and all its child nodes."
  ([node]
   (first (sum-metadata 0 node)))
  ([sum node]
   (sum-metadata sum (first node) (second node) (drop 2 node)))
  ([sum num-children metadata-length data]
   (loop [sum sum
          length 0
          data data
          remaining-children num-children]
     (if (zero? remaining-children)
       [(apply + sum (take metadata-length data)) (+ 2 length metadata-length)]
       (let [[child-sum child-length] (sum-metadata 0 data)]
         (recur (+ sum child-sum)
                (+ length child-length)
                (drop child-length data)
                (dec remaining-children)))))))

(defn sum-metadata-indexes
  [child-sums metadata]
  (->> metadata
       (map dec)
       (map (partial get child-sums))
       (remove nil?)
       (apply +)))

(defn sum-metadata-complicated
  "Sums the metadata of `node` and all its child nodes using the more complicated algorithm."
  ([node]
   (sum-metadata-complicated (first node) (second node) (drop 2 node)))
  ([num-children metadata-length data]
   (if (zero? num-children)
     [(apply + (take metadata-length data)) (+ 2 metadata-length)]
     (loop [child-sums []
            length 0
            data data
            remaining-children num-children]
       (if (zero? remaining-children)
         [(sum-metadata-indexes child-sums (take metadata-length data)) (+ 2 length metadata-length)]
         (let [[child-sum child-length] (sum-metadata-complicated data)]
           (recur (conj child-sums child-sum)
                  (+ length child-length)
                  (drop child-length data)
                  (dec remaining-children))))))))
