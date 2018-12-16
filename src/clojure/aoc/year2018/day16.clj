(ns aoc.year2018.day16)

(defmacro binop
  [name op]
  `(do
     ~(list 'defn (symbol (str name \r))
            '[regs a b c]
            (list 'assoc 'regs 'c (list op '(regs a) '(regs b))))
     ~(list 'defn (symbol (str name \i))
            '[regs a b c]
            (list 'assoc 'regs 'c (list op '(regs a) 'b)))))

(binop add +)
(binop mul *)
(binop ban bit-and)
(binop bor bit-or)

