(ns aoc.year2017.day18
  (:require [clojure.core.async :as async]))

(defn set-register
  "Sets the value of register `reg` inside `registers` to `val`."
  [reg val registers]
  (assoc registers reg val))

(defn get-register
  "Gets the value of register `reg` from `registers`. The default value for a register is 0."
  [reg registers]
  (reg registers 0))

(defn update-register
  "Updates the value of register `reg` inside `registers` by applying `fn` to its current value."
  [reg fn registers]
  (assoc registers reg (fn (get-register reg registers))))

(defn inc-pc
  [registers]
  (set-register :pc (inc (:pc registers 0)) registers))

(defn value-of
  [x registers]
  (if (keyword? x)
    (get-register x registers)
    x))

(defn isound
  "Plays a sound with a frequency equal to the value of `x`."
  [registers x]
  (let [x (value-of x registers)]
    (->> registers
         (set-register :output x)
         (inc-pc))))

(defn iset
  "Sets register `x` to the value of `y`."
  [registers x y]
  (let [y (value-of y registers)]
    (->> registers
         (set-register x y)
         (inc-pc))))

(defn iadd
  "Increases register `x` by the value of `y`."
  [registers x y]
  (let [y (value-of y registers)]
    (->> registers
         (update-register x (partial + y))
         (inc-pc))))

(defn imul
  [registers x y]
  (let [y (value-of y registers)]
    (->> registers
         (update-register x (partial * y))
         (inc-pc))))

(defn imod
  "Sets register `x` to the remainder of dividing the value contained in register `x` by the value of `y`."
  [registers x y]
  (let [y (value-of y registers)]
    (->> registers
         (update-register x #(rem % y))
         (inc-pc))))

(defn irecover
  "Recovers the frequency of the last sound played, but only when the value of `x` is not zero."
  [registers x]
  (inc-pc (if (not (zero? (value-of x registers)))
            (set-register :recovered (get-register :output registers) registers)
            registers)))

(defn ijgz
  "Jumps with an offset of the value of `y`, but only if the value of `x` is greater than zero."
  [registers x y]
  (let [x (value-of x registers)
        y (value-of y registers)]
    (if (< 0 x)
      (update-register :pc (partial + y) registers)
      (inc-pc registers))))

(defn integer-or-keyword
  [input]
  (try
    (Integer/parseInt input)
    (catch NumberFormatException _
      (keyword input))))

(defn parse-instruction
  [input]
  (let [[instr & args] (clojure.string/split input #" ")]
    (apply vector (keyword instr) (map integer-or-keyword args))))

(defn parse-instructions
  [input]
  (map parse-instruction (clojure.string/split-lines input)))

(def soundcard-instr-map {:snd isound
                          :set iset
                          :add iadd
                          :mul imul
                          :mod imod
                          :rcv irecover
                          :jgz ijgz})

(defn soundcard-execute
  [instructions break-condition]
  (loop [registers {}]
    (let [pc (get-register :pc registers)]
      (if (and (< -1 (count instructions)) (not (break-condition registers)))
        (let [[instr & args] (nth instructions pc)]
          (recur (apply (soundcard-instr-map instr) registers args)))
        registers))))

(defn- chan?
  [x]
  (satisfies? clojure.core.async.impl.protocols/WritePort x))

(defn isend
  "Sends the value of `x` to the other program.
  These values wait in a queue until that program is ready to receive them.
  Each program has its own message queue, so a program can never receive a
  message it sent."
  [registers x]
  {:pre [(chan? (get-register :out registers))]}
  (do (async/go (async/>! (get-register :out registers) (value-of x registers)))
      registers))

(defn ireceive
  "Receives the next value and stores it in register `x`.
  If no values are in the queue, the program waits for a value to be sent to it.
  Programs do not continue to the next instruction until they have received a
  value. Values are received in the order they are sent."
  [registers x]
  {:pre (chan? (get-register :in registers))}
  (set-register x
                (async/<!! (async/go (async/<! (get-register :in registers))))
                registers))
