(defn prime?
  "Returns true if n is a prime number, otherwise returns false."
  [n]
  (if (< n 2)
    false
    (loop [i 2]
      (if (<= (* i i) n)
        (if (zero? (rem n i))
          false
          (recur (inc i)))
        true))))
(defn filterp
  [n]
  (filter prime? n))

(defn prime-sum
  [num_p]
  (let [part (/ 5000000 num_p)]
    (pmap filterp part )))