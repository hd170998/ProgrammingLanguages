;----------------------------------------------------------
; Problem Set #2
; Date: March 12, 2020.
; Authors: Hector David Hernandez Rodriguez A01374009
;----------------------------------------------------------

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [abs]])

(defn replic
  "The function replic takes two arguments: a list lst and an integer number n, where n ≥ 0. It returns a new list
   that replicates n times each element contained in lst"
  [number lst]
  (if (<= number 0)
    ()
    (if (<= (count lst) 0)
      ()
      (mapcat #(repeat number %) lst))))
(defn expand
  "The function expand takes a list lst as its argument. It returns a list where the first element of lst appears one
  time, the second elements appears two times, the third element appears three times, and so on."
  [lst]
  (mapcat repeat (range 1 (+ (count lst) 1)) lst))
(defn insert
  "The function insert takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new
  list with the same elements as lst but inserting n in its corresponding place."
  [item lst]
  (concat (filter #(> item %) lst) (list item) (filter #(<= item %) lst)))
(defn sort-auxiliar
  "auxiliar Function of my-sort"
  [aLst bLst]
  (if (empty? bLst)
    aLst
    (sort-auxiliar (insert (first bLst) aLst) (rest bLst))))
(defn my-sort
  "Takes an unordered list and returns a new list with the same elements but in ascending order."
  [lst]
  (sort-auxiliar '() lst))

(defn rotate-left
  "The function rotate-left takes two arguments: an integer number n and a list lst. It returns the list that results
   from rotating lst a total of n elements to the left. If n is negative, it rotates to the right."
  [number lst]
  (if (empty? lst)
    lst
    (if (= number 0)
      lst
      (if (> (abs number) (count lst))
        (if (> number 0)
          (mapcat concat (reverse (split-at (mod (abs number) (count lst)) lst)))
          (mapcat concat (reverse (split-at (- (count lst) (mod (abs number) (count lst))) lst))))
        (if (> number 0)
          (mapcat concat (reverse (split-at number lst)))
          (mapcat concat (reverse (split-at (+ (count lst) number) lst))))))))
(defn binary
  "The function binary takes an integer n as input (assume that n ≥ 0). If n is equal to zero, it returns an empty list.
   If n is greater than zero, it returns a list with a sequence of ones and zeros equivalent to the binary
   representation of n."
  [n]
  (loop [r ()
         n n]
    (if (zero? n)
      r
      (recur (cons (rem n 2 ) r)
             (quot n 2)))))


(defn prime-factors
  "The function prime-factors takes an integer n as input (assume that n > 0), and returns a list containing the prime
   factors of n in ascending order. The prime factors are the prime numbers that divide a number exactly. If you
   multiply all the prime factors you get the original number."
  [num]
  (loop [n num
         i 2
         res '()]
    (if (= (reduce *' res) num)
      (reverse res)
      (if (= (mod n i) 0)
        (recur (/ n i)
               i (cons i res))
        (recur n  (inc i)
               res)))))
(defn gcd [n m]
  "The function gcd takes two positive integer arguments a and b as arguments, where a > 0 and b > 0. It returns the
  greatest common divisor (GCD) of a and b."
  (if (= m 0)
    n
    (recur m (mod n m))))
(defn insert-everywhere
  "The function insert-everywhere takes two arguments as input: an object x and a list lst. It returns a new list with
   all the possible ways in which x can be inserted into every position of lst."
  [item lst]

  (map #
           (concat (first %) (list item) (last %))
       (map #(split-at % lst) (range 0 (+ (count lst) 1)))))
(defn deep-reverse
  "The function deep-reverse takes a list as its input. It returns a list with the same elements as its input but in
  reverse order. If there are any nested lists, these too should be reversed."
  [lst]
  (reverse (map #(if (list? %) (deep-reverse %) %) lst)))
(defn pack
  "The function pack takes a list lst as its argument. If lst contains consecutive repeated elements they should be
   placed in separate sublists."
  [lst]
  (partition-by identity lst))
(defn compress
  "The function compress takes a list lst as its argument. If lst contains consecutive repeated elements, they should
   be replaced with a single copy of the element. The order of the elements should not be changed."
  [lst]
  (map #(first %) (pack lst)))
(defn encode
  "The function encode takes a list lst as its argument. Consecutive duplicates of elements in lst are encoded as
  vectors [n e], where n is the number of duplicates of the element e. "
  [lst]
  (let [a (map #(count %) (pack lst))
        b (compress lst)]
    (map #(into [] %) (partition 2 (interleave a b)))))

(defn encode-modified
  "The function encode-modified takes a list lst as its argument. It works the same as the previous problem, but if
   an element has no duplicates it is simply copied into the result list. "
  [lst]
  (let [a (map #(count %) (pack lst))
        b (compress lst)]
    (map #(if (= (first %)1)
            (last %)
            (into [] %))(partition 2 (interleave a b)))))
(defn decode
  "The function decode takes as its argument an encoded list lst that has the same structure as the resulting list
  from the previous problem. It returns the decoded version of lst."
  [lst]
  (flatten (map #(if (vector? %) (repeat (first %) (last %)) %) lst)))


;-------------TESTS--------------------------
(deftest test-repli
  (is (= () (replic 7 ())))
  (is (= () (replic 0 '(a b c))))
  (is (= '(a a a) (replic 3 '(a))))
  (is (= '(1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4)
         (replic 4 '(1 2 3 4)))))
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))
(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))
(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))
(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)