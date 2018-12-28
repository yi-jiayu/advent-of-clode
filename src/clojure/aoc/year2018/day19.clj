(ns aoc.year2018.day19
  (:require [aoc.year2018.day16]))

(def opcodes {:addi aoc.year2018.day16/addi
              :addr aoc.year2018.day16/addr
              :bani aoc.year2018.day16/bani
              :banr aoc.year2018.day16/banr
              :bori aoc.year2018.day16/bori
              :borr aoc.year2018.day16/borr
              :eqir aoc.year2018.day16/eqir
              :eqri aoc.year2018.day16/eqri
              :eqrr aoc.year2018.day16/eqrr
              :gtir aoc.year2018.day16/gtir
              :gtri aoc.year2018.day16/gtri
              :gtrr aoc.year2018.day16/gtrr
              :muli aoc.year2018.day16/muli
              :mulr aoc.year2018.day16/mulr
              :seti aoc.year2018.day16/seti
              :setr aoc.year2018.day16/setr})

(defn parse-instruction
  [instr]
  (let [[opcode & args] (clojure.string/split instr #" ")]
    (apply conj [(keyword opcode)] (map #(Integer/parseInt %) args))))

(defn parse-instructions
  "Loads a program."
  [instructions]
  (->> instructions
       (map clojure.string/trim)
       (mapv parse-instruction)))

(defn parse-declaration
  [decl]
  (let [[_ val] (-> decl
                    (subs 1)
                    (clojure.string/split #" "))]
    (Integer/parseInt val)))

(defn parse-input
  [input]
  (let [[decl & prog] (-> input
                          clojure.string/trim
                          clojure.string/split-lines)]
    {:ip-binding (parse-declaration decl)
     :program    (parse-instructions prog)}))

(defn execute-instruction
  [{:keys [ip ip-binding registers] :as state} [instr & args]]
  (let [regs' (as-> registers regs
                    (assoc regs ip-binding ip)
                    (apply (opcodes instr) regs args))
        ip (inc (nth regs' ip-binding))]
    (-> state
        (assoc :ip ip)
        (assoc :registers regs'))))

(defn run-program
  ([input]
   (run-program input 0 [0 0 0 0 0 0]))
  ([{:keys [ip-binding program]} ip registers]
   (let [state {:ip-binding ip-binding
                :ip         ip
                :registers  registers}]
     (loop [state state]
       (let [{ip :ip} state]
         (if-let [next-instr (get program ip)]
           (recur (execute-instruction state next-instr))
           state))))))

(defn factorise
  "Returns all the factors of `n`."
  [n]
  (->> n
       inc
       (range 1)
       (filter #(zero? (rem n %)))))
