(ns aoc.year2018.day16
  (:require [clojure.spec.alpha :as s]))

(s/def ::register? (s/int-in 0 4))
(s/def ::registers? (s/coll-of integer? :count 4))

(defmacro binoprr
  [name op doc]
  (list 'do
        (list 'defn name
              doc
              '[regs a b c]
              (list 'assoc 'regs 'c (list op '(regs a) '(regs b))))
        (list 's/fdef name
              :args '(s/cat :regs ::registers? :a ::register? :b ::register? :c ::register?)
              :ret ::registers?)))

(defmacro binopir
  [name op doc]
  (list 'do
        (list 'defn name
              doc
              '[regs a b c]
              (list 'assoc 'regs 'c (list op 'a '(regs b))))
        (list 's/fdef name
              :args '(s/cat :regs ::registers? :a integer? :b ::register? :c ::register?)
              :ret ::registers?)))

(defmacro binopri
  [name op doc]
  (list 'do
        (list 'defn name
              doc
              '[regs a b c]
              (list 'assoc 'regs 'c (list op '(regs a) 'b)))
        (list 's/fdef name
              :args '(s/cat :regs ::registers? :a ::register? :b integer? :c ::register?)
              :ret ::registers?)))

(defmacro unopr
  [name op doc]
  (list 'do
        (list 'defn name
              doc
              '[regs a _ c]
              (list 'assoc 'regs 'c (list op '(regs a))))
        (list 's/fdef name
              :args '(s/cat :regs ::registers? :a ::register? :b any? :c ::register?)
              :ret ::registers?)))

(defmacro unopi
  [name op doc]
  (list 'do
        (list 'defn name
              doc
              '[regs a _ c]
              (list 'assoc 'regs 'c (list op 'a)))
        (list 's/fdef name
              :args '(s/and (s/cat :regs ::registers? :a integer? :b any? :c ::register?))
              :ret ::registers?)))

(binoprr addr +'
  "addr (add register) stores into register C the result of adding register A
  and register B.")

(binopri addi +'
  "addi (add immediate) stores into register C the result of adding register A
  and value B.")

(binoprr mulr *'
  "mulr (multiply register) stores into register C the result of multiplying
  register A and register B.")

(binopri muli *'
  "muli (multiply immediate) stores into register C the result of multiplying
  register A and value B.")

(binoprr banr bit-and
  "(bitwise AND register) stores into register C the result of the bitwise AND
  of register A and register B.")

(binopri bani bit-and
  "(bitwise AND immediate) stores into register C the result of the bitwise AND
  of register A and value B.")

(binoprr borr bit-or
  "borr (bitwise OR register) stores into register C the result of the bitwise
  OR of register A and register B.")

(binopri bori bit-or
  "bori (bitwise OR immediate) stores into register C the result of the bitwise
  OR of register A and value B.")

(unopr setr identity
  "setr (set register) copies the contents of register A into register C.
  (Input B is ignored.)")

(unopi seti identity
  "seti (set immediate) stores value A into register C. (Input B is ignored.)")

(binopir gtir (fn [a b] (if (> a b) 1 0))
  "gtir (greater-than immediate/register) sets register C to 1 if value A is
  greater than register B. Otherwise, register C is set to 0.")

(binopri gtri (fn [a b] (if (> a b) 1 0))
  "gtri (greater-than register/immediate) sets register C to 1 if register A is
  greater than value B. Otherwise, register C is set to 0.")

(binoprr gtrr (fn [a b] (if (> a b) 1 0))
  "gtrr (greater-than register/register) sets register C to 1 if register A is
  greater than register B. Otherwise, register C is set to 0.")

(binopir eqir (fn [a b] (if (= a b) 1 0))
  "eqir (equal immediate/register) sets register C to 1 if value A is equal to
  register B. Otherwise, register C is set to 0.")

(binopri eqri (fn [a b] (if (= a b) 1 0))
  "eqri (equal register/immediate) sets register C to 1 if register A is equal
  to value B. Otherwise, register C is set to 0.")

(binoprr eqrr (fn [a b] (if (= a b) 1 0))
  "eqrr (equal register/register) sets register C to 1 if register A is equal
  to register B. Otherwise, register C is set to 0.")

(def opcodes [eqrr eqir addr addi borr seti gtrr banr
              setr muli mulr eqri gtri bani bori gtir])

(defrecord Sample [instruction before after])

(defn parse-instruction
  [instr]
  (mapv #(Integer/parseInt %) (clojure.string/split instr #" ")))

(defn parse-sample
  "Returns the instruction sampled as well as the registers before and after."
  [sample]
  (let [[line1 line2 line3] (->> sample
                                 clojure.string/trim
                                 clojure.string/split-lines
                                 (map clojure.string/trim))
        before (as-> line1 %%
                     (re-matches #"Before: \[(.+)\]" %%)
                     (clojure.string/split (second %%) #", ")
                     (mapv #(Integer/parseInt %) %%))
        instruction (parse-instruction line2)
        after (as-> line3 %%
                    (re-matches #"After:  \[(.+)\]" %%)
                    (clojure.string/split (second %%) #", ")
                    (mapv #(Integer/parseInt %) %%))]
    (->Sample instruction before after)))

(defn parse-samples
  "Returns all the samples in input."
  [input]
  (as-> input %
        (clojure.string/trim %)
        (clojure.string/split % #"\n\n")
        (map parse-sample %)))

(defn parse-program
  "Returns a list of instructions."
  [program]
  (->> program
       clojure.string/trim
       clojure.string/split-lines
       (map #(clojure.string/trim %))
       (mapv parse-instruction)))

(defn parse-input
  "Returns a map containing the samples and sample program found in input."
  [input]
  (let [[samples program] (clojure.string/split input #"\n\n\n\n")]
    {:samples (parse-samples samples)
     :program (parse-program program)}))

(defn matches?
  "Returns true if (opcode regs a b c) is equal to expected."
  [opcode regs [a b c] expected]
  (= (opcode regs a b c) expected))

(defn matching-opcodes
  "Returns the opcodes which could have produced the result seen in sample."
  [{[_ a b c] :instruction before :before after :after} opcodes]
  (into #{} (filter #(matches? % before [a b c] after) opcodes)))

(defn count-matching-opcodes
  "Returns the number of opcodes which could have produced the result seen in sample."
  [sample opcodes]
  (count (matching-opcodes sample opcodes)))

(defn matches-per-sample
  "Returns the number of matches for each sample in samples."
  [samples]
  (map #(count-matching-opcodes % opcodes) samples))

(defn solve-for-opcodes
  "Determines which number each opcode corresponds to based on samples."
  [opcodes samples]
  (loop [remaining-opcodes (into #{} opcodes)
         numbers-to-opcodes {}
         solved-opcodes #{}]
    (if (= (count opcodes) (count solved-opcodes))
      numbers-to-opcodes
      (let [unsolved-opcodes (remove (fn [{[number] :instruction}] (solved-opcodes number)) samples)
            [number candidates] (->> unsolved-opcodes
                                     (map (fn [{[number] :instruction :as sample}] [number (matching-opcodes sample remaining-opcodes)]))
                                     (filter (fn [[_ candidates]] (= 1 (count candidates))))
                                     first)
            opcode (first candidates)]
        (recur (disj remaining-opcodes opcode)
               (assoc numbers-to-opcodes number opcode)
               (conj solved-opcodes number))))))

(defn run-program
  "Runs program using the given opcode map."
  [opcode-map program]
  (reduce
    (fn [regs [opcode a b c]] ((opcode-map opcode) regs a b c))
    [0 0 0 0]
    program))
