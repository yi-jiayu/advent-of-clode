(ns aoc.year2017.day17
  (:import (java.util LinkedList)))

(defn spinlock-insert
  ([step-size ^LinkedList buf curr-pos next-value]
   (let [buf-size (count buf)
         new-pos (inc (rem (+ curr-pos step-size) buf-size))]
     (.add buf new-pos next-value)
     [buf new-pos])))

(defn spinlock
  [rounds step-size]
  (let [buf (new LinkedList [0])]
    (loop [buf buf
           curr-pos 0
           next-value 1]
      (if (< rounds (count buf))
        [buf curr-pos]
        (let [[buf new-pos] (spinlock-insert step-size buf curr-pos next-value)]
          (recur buf
                 new-pos
                 (inc next-value)))))))

(defn short-circuit
  [rounds step-size]
  (let [[^LinkedList buf last-index] (spinlock rounds step-size)]
    (.get buf (inc last-index))))
