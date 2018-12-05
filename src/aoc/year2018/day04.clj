(ns aoc.year2018.day04
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDateTime)
           (java.time.temporal ChronoUnit)))

(def timestamp-formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn parse-log-line
  [line]
  (-> (->> line
           (re-matches #"\[(.+)] (?:Guard #(\d+) begins shift|(falls asleep)|(wakes up))")
           (rest)
           (zipmap [:timestamp :guard :falls-asleep :wakes-up]))
      (update :timestamp #(LocalDateTime/parse % timestamp-formatter))
      (update :guard #(if (nil? %) % (Integer/parseInt %)))))

(defn parse-logs
  [logs]
  (sort-by :timestamp
           (map (comp parse-log-line
                      clojure.string/trim)
                (->> logs
                     (clojure.string/trim)
                     (clojure.string/split-lines)))))

(defn minutes-between
  [start end]
  (.until start end ChronoUnit/MINUTES))

(defn update-total-minutes-slept
  [total-minutes-slept guard-id time-slept time-woke]
  (let [duration-slept (minutes-between time-slept time-woke)]
    (update total-minutes-slept guard-id (fnil (partial + duration-slept) 0))))

(defn calculate-total-minutes-slept
  [logs]
  (loop [total-minutes-slept {}
         guard-id nil
         last-time nil
         logs logs]
    (if (empty? logs)
      total-minutes-slept
      (let [curr (first logs)
            [total-minutes-slept
             guard-id
             last-time] (condp not= nil
                          (:guard curr) [total-minutes-slept
                                         (:guard curr)
                                         nil]
                          (:falls-asleep curr) [total-minutes-slept
                                                guard-id
                                                (:timestamp curr)]
                          (:wakes-up curr) [(update-total-minutes-slept total-minutes-slept
                                                                        guard-id
                                                                        last-time
                                                                        (:timestamp curr))
                                            guard-id
                                            nil])]
        (recur total-minutes-slept
               guard-id
               last-time
               (rest logs))))))

(defn laziest-guard
  [minutes-slept]
  (key (apply max-key val minutes-slept)))

(defn update-minutes-asleep-count
  [count time-slept time-woke]
  (map-indexed (fn [i x]
                 (if (<= (.getMinute time-slept) i (- (.getMinute time-woke) 1))
                   (inc x)
                   x))
               count))

(defn calculate-minutes-asleep-count
  [logs guard-id]
  (loop [minutes-asleep-count (repeat 60 0)
         guard-id' nil
         last-time nil
         logs logs]
    (if (empty? logs)
      minutes-asleep-count
      (let [curr (first logs)
            [total-minutes-slept
             guard-id'
             last-time] (condp not= nil
                          (:guard curr) [minutes-asleep-count
                                         (:guard curr)
                                         nil]
                          (:falls-asleep curr) [minutes-asleep-count
                                                guard-id'
                                                (:timestamp curr)]
                          (:wakes-up curr) [(if (= guard-id guard-id')
                                              (update-minutes-asleep-count minutes-asleep-count
                                                                           last-time
                                                                           (:timestamp curr))
                                              minutes-asleep-count)
                                            guard-id'
                                            nil])]
        (recur total-minutes-slept
               guard-id'
               last-time
               (rest logs))))))

(defn minute-asleep-most
  [logs guard]
  (let [minutes-asleep-count (calculate-minutes-asleep-count logs guard)]
    (.indexOf minutes-asleep-count (apply max minutes-asleep-count))))

(defn all-guards
  [logs]
  (into #{} (filter (comp not nil?) (map :guard logs))))

(defn most-frequently-asleep-in-the-same-minute
  [logs]
  (let [guards (all-guards logs)
        guard (apply max-key #(apply max (calculate-minutes-asleep-count logs %)) guards)
        minute (minute-asleep-most logs guard)]
    [guard minute]))
