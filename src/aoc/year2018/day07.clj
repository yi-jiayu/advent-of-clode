(ns aoc.year2018.day07)

(defn parse-line
  "Parses one line of the input"
  [line]
  (let [[head tail] (clojure.string/split line #" must be finished before step ")]
    [(last head) (first tail)]))

(defn parse-input
  [input]
  (->> input
       (clojure.string/trim)
       (clojure.string/split-lines)
       (map parse-line)))

(defn all-steps
  "Returns a set of all the unique steps specified in `reqs`."
  [reqs]
  (into #{} (flatten reqs)))

(defn update-dependency-list
  [dlist [before after]]
  (update dlist after #(conj % before)))

(defn build-dependency-list
  "Builds a dependency list out of reqs"
  [reqs]
  (let [dlist (zipmap (all-steps reqs) (repeat #{}))]
    (reduce update-dependency-list dlist reqs)))

(defn available-steps
  "Returns a set of available steps based on `dlist`."
  [dlist]
  (->> dlist
       (filter (comp empty? second))
       (map first)
       (into #{})))

(defn remove-alphabetical-first
  [set]
  (let [first (first (sort set))]
    [first (disj set first)]))

(defn remove-dependency
  [dlist dep]
  (zipmap (keys dlist) (map #(disj % dep) (vals dlist))))

(defn topo-sort
  "Return a topological ordering of the steps specified in `reqs`.
  Ties are broken using alphabetical order."
  [reqs]
  (loop [dlist (build-dependency-list reqs)
         ordered []]
    (if (empty? dlist)
      ordered
      (let [curr (first (sort (available-steps dlist)))
            ordered (conj ordered curr)
            dlist (-> dlist (dissoc curr) (remove-dependency curr))]
        (recur dlist
               ordered)))))
