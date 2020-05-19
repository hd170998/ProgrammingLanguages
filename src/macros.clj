
(defmacro bif   "Binds the condition to a variable.   If that value is true, evaluates and returns   the then-part, otherwise evaluates and returns   the else part."
  [variable condition then-part else-part]
  `(let [~variable ~condition]
     (if ~variable        ~then-part        ~else-part)))


(defn def-vars-helper   "Creates individual (def var-name-suffix expr)   lists."
  [var-name expressions]
  (map (fn [n expr]
         `(def ~(symbol (str var-name n)) ~expr))
       (range) expressions))

(defmacro def-vars   "Defines as many global variables as the provided   number of expressions. The value of var-name is the   name prefix for al these variables. The suffix   is '0' for the first variable name, which is initialized   with the first expression. The next variable name   has a '1' suffix and is initialized with the second   expression, and so on with all remaining variables."
  [var-name & expressions]
  `(do ~@(def-vars-helper var-name expressions)))

(defn def-many-helper
  "Creates individual (def var-name-suffix expr)   lists."
  [var-names expressions]
  (map (fn [name expr]
         `(def ~name ~expr)) var-names expressions))
(defmacro def-many
  "Defines as many global variables as the provided   number of expressions. Each pair of arguments represent   the name of the variable and its expression respectively."
  [& input]
  (let [var-names
        (take-nth 2 input)
        expressions (take-nth 2 (rest input))]
    `(do ~@(def-many-helper var-names expressions))))

(defn nth-helper
  [expressions]
  (mapcat
    (fn [name expr] `(~name ~expr))
       (range) expressions)

  )

(defmacro nth-expr
  [n & expressions]
  `(case ~n
    ~@(nth-helper expressions)

    (throw (java.lang.RuntimeException. "Bad nth value!")))
  )




