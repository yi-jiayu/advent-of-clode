(ns aoc.year2018.day07
  (:require [clojure.set :refer [difference]]))

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

(defn time-required-for-step
  [base step]
  (+ base (- (int step) 64)))

(defn time-required-for-steps
  "Returns a map of each step to the amount of time required to complete it."
  [base steps]
  (into {} (for [step steps] [step (time-required-for-step base step)])))

(defn available?
  [dlist step]
  (empty? (dlist step)))

(defn decrement-time-left
  "Decrement the time left for the given steps."
  [time-left steps]
  (reduce (fn [time-left' step] (update time-left' step dec)) time-left steps))

(defn completed-steps
  "Returns the steps which have no time left."
  [time-left]
  (->> time-left
       (filter (comp zero? second))
       (map first)
       (into #{})))

(defn remove-completed-steps
  "Removes steps with zero time left from `time-left`."
  [time-left]
  (into {} (for [[step left] time-left
                 :when (< 0 left)]
             [step left])))

(defn tick
  [num-workers in-progress dlist steps time-left]
  (let [available-workers (- num-workers (count in-progress))
        ready (take available-workers (->> steps
                                           (filter (available-steps dlist))
                                           (remove in-progress)
                                           (into #{})))
        in-progress (apply conj in-progress ready)
        time-left (decrement-time-left time-left in-progress)
        done (completed-steps time-left)
        steps (remove done steps)
        in-progress (difference in-progress done)
        dlist (apply dissoc dlist done)
        dlist (reduce remove-dependency dlist done)]
    [in-progress dlist steps time-left]))

(defn construct
  "Returns the time taken to fully execute `steps` in order with `num-workers` and given `reqs`."
  [num-workers base reqs steps]
  (loop [in-progress #{}
         dlist (build-dependency-list reqs)
         steps steps
         time-left (time-required-for-steps base (all-steps reqs))
         time 0]
    (if (empty? steps)
      time
      (let [[in-progress dlist steps time-left] (tick num-workers in-progress dlist steps time-left)]
        (recur in-progress
               dlist
               steps
               time-left
               (inc time))))))
