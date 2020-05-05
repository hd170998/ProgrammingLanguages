;----------------------------------------------------------
; Problem Set #3
; Date: April 01, 2020.
; Authors:
;          A01374009 Héctor Hernádez
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn argswap
  "The argswap function takes as input a two argument function f and returns
   a new function that behaves like f but with the order of its two arguments swapped"
  [f]
  (fn [x y]
    (f y x)))


(defn there-exists-one [pos? param2]
  "The function there-exists-one takes two arguments: a one argument predicate function pred and a list lst. Returns
  true if there is exactly one element in lst that satisfies pred, otherwise returns false."
  (if (= (some pos? param2) nil) false (some pos? param2)))

(defn bisection
  "the function bisection, that takes a, b, and f as arguments. It finds the corresponding root using the bisection
  method."
  [a b func]
  (loop [limit-I a
         limit-S b
         root (/ (+ limit-I limit-S) 2)]
    (if (< (abs (func root)) 1.0E-15)
      root
      (if (= (neg? (func limit-I)) (neg? (func root)))
        (recur root limit-S (/ (+ root limit-S) 2))
        (recur limit-I root (/ (+ limit-I root) 2))))))

(defn linear-search [param1 param2 func]
  (let [res (.indexOf (map #(func param2 %) param1) true)]
    (if (= -1 res) nil res)))

(defn deriv
  "the function deriv that takes f and h as its arguments, and returns a new function that takes x as argument, and
   which represents the derivative of f given a certain value for h"
  [func h]
  #(/ (- (func (+ % h)) (func %)) h))

(defn newton
  "the function newton that takes f and n as its arguments, and returns the corresponding value of Xn"
  [func n]
  (loop [iter 0
         x 0
         fx (func x)
         dfx ((deriv func 0.001) x)
         xn (- x (/ fx dfx))]
    (if (= iter n)
      x
      (recur (inc iter)
             xn
             (func xn)
             ((deriv func 0.001) xn)
             (- xn (/ (func xn) ((deriv func 0.001) xn)))))))

(defn integral
  "the function integral, that takes as arguments a, b, n, and f. It returns the value of the integral,
   using Simpson’s rule. "
  [a b n func]
  (let [h (/ (- b a) n)
        lstY (loop [lst '()
                    k 0]
               (if (> k n)
                 (reverse lst)
                 (recur (cons (func (+ a (* k h))) lst) (inc k))))]
    (* (/ h 3) (reduce + (map (fn [x] (if (or (= (first lstY) x) (= (last lstY) x))
                                        x (if (= (mod (.indexOf lstY x) 2) 0)
                                            (* x 2) (* x 4)))) lstY)))))

(defn binary-search
  "The binary-search function returns the index where x is found in vct (the first element of the vector is at index 0),
   or nil if not found."
  [vct x bs-fun]
  (let [eq-fun (fn [a b] (and (not (bs-fun a b)) (not (bs-fun b a))))]
    (loop [findVect vct
           mid (int (/ (dec (count findVect)) 2))]
      (if (empty? findVect)
        nil
        (if (eq-fun x (nth findVect mid))
          (.indexOf vct x)
          (if (bs-fun x (nth findVect mid))

            (recur (first (split-at mid findVect))
                   (int (/ (dec (count (first (split-at mid findVect)))) 2)))
            (recur (last (split-at (inc mid) findVect))
                   (int (/ (dec (count (last (split-at (inc mid) findVect)))) 2)))))))))


(deftest test-argswap
  (is (= '(2 "1")
         ((argswap list) "1" 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))


(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))
(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(deftest test-newton
  (is (aprox= 0.00001
              10.0
              (newton (fn [x] (- x 10))
                      1)))
  (is (aprox= 0.00001
              -0.5
              (newton (fn [x] (+ (* 4 x) 2))
                      1)))
  (is (aprox= 0.00001
              -1.0
              (newton (fn [x] (+ (* x x x) 1))
                      50)))
  (is (aprox= 0.00001
              -1.02987
              (newton (fn [x] (+ (Math/cos x)
                                 (* 0.5 x)))
                      5))))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4 (integral 1 2 10 (fn [x] (integral 3 4 10 (fn [y] (* x y))))))))

(def small-list [4 8 15 16 23 42])
(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])

(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))

(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))

(run-tests)