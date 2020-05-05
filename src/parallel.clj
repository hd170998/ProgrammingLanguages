;----------------------------------------------------------
; Classwork: Using Parallel Map
; April 13, 2020.
; Breakout Room:
;          A01378562 Karla Lopez Vega
;          A01374009 Hector Hernandez Rodriguez
;          A01374866 Roberto Tellez Perezyera
;----------------------------------------------------------

; auxiliary function provided in instruction document
(defn prime?
  "Returns true if n is a prime number, otherwise returns false."
  [n]
  (if (< n 2)
    false (loop [i 2]
            (if (<= (* i i) n)
              (if (zero? (rem n i))
                false
                (recur (inc i))) true))))


(defn prime-chunks
  "Returns how-many two-element lists, which contain
  the start and end values that will be passed to the
  compute-prime-sum auxiliary function."
  [how-many]
  (->>
    (range 0 (inc 5000000) (/ 5000000 how-many))
    (partition 2 1)))


(defn compute-prime-sum
  "Takes a start and end values as arguments.
  Returns the sum of all prime numbers found
  between start and end."
  [start end]
  (loop [accum 0
         lst (range start (inc end))
         curr (first lst)]
    (if (empty? lst)
      accum
      (recur (if (prime? curr)
               (+ accum curr)
               accum)
             (rest lst)
             (first (rest lst))))))


(defn prime-sum
  "Takes an integer representing the number of
  threads to use as an argument.
  Using Clojure's pmap function, returns
  the sum of all the primes below five million
  computing the solution in parallel."
  [threads]
  (->>
    ; this is the last argument for pmap on line 62
    (prime-chunks threads)

    ; apply the function compute-prime-sum to
    ; the n lists of two elements that prime-chunks returns
    (pmap (fn [[start end]]
            (compute-prime-sum start end)))

    ; now we have n "partial sums", add those up
    ; for the final result
    (reduce +)))

(fn squr[x]
  (< x
     (apply +
            (map
              #(apply * (repeat 2 (- (int %) (int \0))))
              (seq (str x))))))

(count (filter squr (range 10)))