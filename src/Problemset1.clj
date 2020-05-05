; Problem Set #1
; @author A01374009 Hector David Hernandez Rodriguez
;----------------------------------------------------------
(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])
(require '[clojure.math.numeric-tower :refer [sqrt]])

(defn !
  "Computes the factorial of n using explicit recursion."
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))

(defn duplicate
  "The function duplicate takes a list lst as its argument and returns a new list with each element of lst duplicated."
  [lst]
  (mapcat #(list % %) lst))

(defn pow
  "The function pow takes two arguments as input: a number a and a positive integer b. It returns the result of
   computing a raised to the power b."
  [a b]
  (reduce *' (repeat b a)))

(defn fib
  "The function fib takes a positive integer n as its argument and returns the corresponding
  element of the Fibonacci sequence"
  [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b
             (+' a b)
             (+' 1 i)))))

(defn enlist
  "The function enlist surrounds in a list every upper-level element of the list it takes as input"
  [lst]
  (map #(list %) lst))

(defn positives
  "The function positives takes a list of numbers lst as its argument, and returns a new list that only
  contains the positive numbers of lst"
  [lst]
  (filter pos? lst))

(defn add-list
  "The function add-list returns the sum of all the elements of its input list, or 0 if its empty.
  Assume that all the elements in the input list are numbers. "
  [lst]
  (reduce + lst))

(defn invert-pairs
  "The function invert-pairs takes as an argument a list of vectors containing two elements each.
  It returns a new list with every vector pair inverted."
  [lst]
  (map #(reverse %) lst))

(defn list-of-symbols?
  "The function list-of-symbols? takes a list lst as its argument. It returns true if all the elements (possibly zero)
   contained in lst are symbols, or false otherwise. Use the symbol? predicate to determine if something is a symbol."
  [lst]
  (= (count (drop-while symbol? lst)) 0))

(defn swapper
  "The function swapper takes three arguments as input: two values x and y, and a list lst. It returns a new list in
   which every occurrence of x in lst is swapped with y, and vice versa. Any other element of lst remains the same.
   You may assume that lst does not contain nested sequences"
  [a b lst]
  (map (fn [e]
         (cond
           (= a e) b
           (= b e) a
           :else e))
       lst))

(defn dot-product
  "The function dot-product takes two arguments: the lists a and b. It returns the result of performing the dot product
  of a times b. The dot product is an algebraic operation that takes two equal-length sequences of numbers and returns
  a single number obtained by multiplying corresponding entries and then summing those products."
  [lista1 lista2]
  (reduce +' (map *' lista1 lista2)))

(defn average
  "The function average takes a list lst as its argument. It returns the arithmetic mean of the numbers contained in
  lst, or nil if lst is empty."
  [lst]
  (if (= (count lst) 0)
    nil
    (/ (reduce + lst) (count lst))))

(defn delete-all
  [x lst]
  (cond
    (empty? lst)
    ()
    (list? (fist lst))
    (delete-all x (fist lst))
    (delete-all x (rest lst))))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))
(defn standard-deviation
  "The function standard-deviation takes a list lst as its argument. It returns the population standard deviation of
  the numbers contained in lst, or nil if lst is empty. "
  [lst]
  (if (= (count lst) 0)
    nil
    (sqrt (/ (reduce + (map #(pow (- % (average lst)) 2) lst)) (count lst)))))


;-----------------TESTS------------------------
(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
         (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))
(deftest test-duplicate
  (is (= '(1 1 2 2 3 3 4 4 5 5)
         (duplicate '(1 2 3 4 5))))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))
(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))
(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
            987 1597 2584 4181 6765)
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))
(deftest test-positives
  (is (= () (positives '())))
  (is (= () (positives '(-4 -1 -10 -13 -5))))
  (is (= '(3 6) (positives '(-4 3 -1 -10 -13 6 -5))))
  (is (= '(4 3 1 10 13 6 5) (positives '(4 3 1 10 13 6 5)))))
(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
      (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))
(deftest test-dot-product
  (is (= 0 (dot-product () ())))
  (is (= 32 (dot-product '(1 2 3) '(4 5 6))))
  (is (= 21.45 (dot-product '(1.3 3.4 5.7 9.5 10.4)
                            '(-4.5 3.0 1.5 0.9 0.0)))))
(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))
(deftest test-swapper
  (is (= ()
         (swapper 1 2 ())))
  (is (= '(4 3 4 9 9 3 3 3 9 9 7 9
            3 7 8 7 8 4 5 6)
         (swapper 1 2 [4 3 4 9 9 3 3 3 9 9 7
                       9 3 7 8 7 8 4 5 6])))
  (is (= '(4 4 5 1 4 8 1 5 6 4 5 2 9 5 9 9 2 1 1 4)
         (swapper 1 2 [4 4 5 2 4 8 2 5 6 4 5
                       1 9 5 9 9 1 2 2 4])))
  (is (= '(soft purr warm purr little ball of fur
                happy purr sleepy purr kitty kitty kitty)
         (swapper 'purr
                  'kitty
                  '(soft kitty warm kitty little ball
                         of fur happy kitty sleepy kitty
                         purr purr purr)))))
(deftest test-average
  (is (nil? (average ())))
  (is (= 4
         (average '(4))))
  (is (= 3
         (average '(5 6 1 6 0 1 2))))
  (is (= 2.5
         (average '(1.7 4.5 0 2.0 3.4 5 2.5 2.2 1.2)))))

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))
(deftest test-standard-deviation
  (is (nil? (standard-deviation ())))
  (is (aprox= 0.01
              1.87
              (standard-deviation
                '(6 2 3 1))))
  (is (aprox= 0.0001
              12.3153
              (standard-deviation
                '(4 8 15 16 23 42))))
  (is (aprox= 0.00001
              7.07106
              (standard-deviation
                '(110 105 90 100 95))))
  (is (aprox= 0.001
              2.983
              (standard-deviation
                '(9 2 5 4 12 7 8 11
                   9 3 7 4 12 5 4 10
                   9 6 9 4)))))
(run-tests)
