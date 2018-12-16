(ns aoc.year2018.day16
  (:require [clojure.spec.alpha :as s]))

(s/def ::register? (s/int-in 0 4))
(s/def ::registers? (s/coll-of integer? :count 4))

(defmacro binoprr
  [name op doc]
  (list 'defn name
        doc
        '[regs a b c]
        (list 'assoc 'regs 'c (list op '(regs a) '(regs b)))))


(defmacro binopir
  [name op doc]
  (list 'defn name
        doc
        '[regs a b c]
        (list 'assoc 'regs 'c (list op 'a '(regs b)))))

(defmacro binopri
  [name op doc]
  (list 'defn name
        doc
        '[regs a b c]
        (list 'assoc 'regs 'c (list op '(regs a) 'b))))

(binoprr addr +'
  "addr (add register) stores into register C the result of adding register A and register B.")
(s/fdef addr
        :args (s/and (s/cat :regs ::registers? :a ::register? :b ::register? :c ::register?))
        :ret ::registers?
        :fn (fn [{{:keys [regs a b c]} :args ret :ret}]
              (let [expected (+' (regs a) (regs b))]
                (= (assoc regs c expected) ret))))

(binopri addi +
  "addi (add immediate) stores into register C the result of adding register A and value B.")

(binoprr mulr *
  "mulr (multiply register) stores into register C the result of multiplying register A and register B.")
(binopri muli *
  "muli (multiply immediate) stores into register C the result of multiplying register A and value B.")

(binoprr banr bit-and
  "(bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.")
(binopri bani bit-and
  "(bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.")

(binoprr borr bit-or
  "borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.")
(binopri bori bit-or
  "bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.")

(binopri setr (fn [a _] a)
  "setr (set register) copies the contents of register A into register C. (Input B is ignored.)")
(binopir seti (fn [a _] a)
  "seti (set immediate) stores value A into register C. (Input B is ignored.)")

(binopir gtir (fn [a b] (if (> a b) 1 0))
  "gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.")

(binopri gtri (fn [a b] (if (> a b) 1 0))
  "gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.")

(binoprr gtrr (fn [a b] (if (> a b) 1 0))
  "gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.")

(binopir eqir (fn [a b] (if (= a b) 1 0))
  "eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.")

(binopri eqri (fn [a b] (if (= a b) 1 0))
  "eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.")

(binoprr eqrr (fn [a b] (if (= a b) 1 0))
  "eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.")
