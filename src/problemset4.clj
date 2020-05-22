;----------------------------------------------------------
; Problem Set #4
; Date: May 21, 2020.
; Authors: Hector David Hernandez Rodriguez A01374009
;----------------------------------------------------------

(defmacro my-or
  "Returns the same functionality of or in Clojure with an own implementation."
  ([] nil)
  ([a] a)
  ([a & next]
   `(let [t# ~a]
      (if t#
        t# (my-or ~@next)))))


(defn seek
  "Return what type of loop the function needs to iterate."
  [args]
  (if (== (compare (first args) :until) 0)
    (list (concat '(not) (rest args)))
    (rest args)))

(defmacro do-loop
  "Combines the functionality of C's do-while statement and Pascal's repeat-until statement."
  [args increment condition]
  `(while ~@(seek condition)
     (do ~args ~increment)))

(defn concat-not
  [name]
  (symbol (str "not-" name)))

(defmacro def-pred
  "Defines two predicate functions: a regular one and its negated version."
  [name args & body]
  `(do (defn ~name ~args ~@body)
       (defn ~(concat-not name) ~args (not (do ~@body)))))



(defn curry-maker
  "Return a list of nested functions of the args."
  [params body]
  (loop [arg (reverse params)
         lst body]
    (if (empty? arg)
      lst
      (let [tmp lst]
        (recur (rest arg) (concat `(fn ~[(first arg)]) (list tmp)))))))

(defmacro defn-curry
  "Performs a currying transformation to a function definition."
  [name args & body]
  (do (concat `(defn ~name ~(if (= (first args) nil) args [(first args)]))
              (list (curry-maker (rest args) `(do ~@body))))))



(defn seek-delimited
  "Returns a sequence with the elements contained in args that are between start and end "
  [args start end]
  (->>
    args
    (drop-while #(not= % start))
    rest
    (take-while #(not= % end))))



(defmacro IF
  "Provides a conditional statement that is syntactically a bit more similar to those found in
  languages like Pascal or Fortran. It has the following form:
  (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
  [condition & args]
  `(if ~condition
     (do ~@(seek-delimited args :THEN :ELSE))
     (do ~@(seek-delimited args :ELSE :THEN))))