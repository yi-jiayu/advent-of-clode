(ns aoc.year2018.day01)

(defn parse-frequency-changes
  [input]
  (->> input
       (clojure.string/trim)
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(defn resulting-frequency
  "Calculates the resulting frequency after all of the changes in frequency
  have been applied."
  [changes]
  (apply + changes))

(defn lazy-frequencies
  "returns a lazy sequence of the frequencies reached while continuously
  repeating the same list of frequency changes."
  [changes]
  (when-let [changes (cycle changes)]
    (map first
         (iterate (fn [[freq changes]]
                    [(+ freq (first changes)) (rest changes)])
                  [0 changes]))))

(defn first-frequency-reached-twice
  [changes]
  (loop [reached #{}
         frequencies (lazy-frequencies changes)]
    (let [curr (first frequencies)]
      (if (contains? reached curr)
        curr
        (recur (conj reached curr)
               (rest frequencies))))))
