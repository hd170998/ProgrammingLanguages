(defn grid
  [s t n]
  (doseq [i (range t)]
    (doseq [j (range (+ (* s n) (+ t (* n t ))))]
      (print "*"))
    (println))
  (doseq [y (range n)]
    (doseq [i (range s)]
      (doseq [m (range n)]
        (doseq [j (range t)]
          (print "*"))
        (doseq [k (range s)]
          (print ".")))
      (doseq [x (range t)]
        (print "*"))
      (println))
    (doseq [i (range t)]
      (doseq [j (range (+ (* s n) (+ t (* n t ))))]
        (print "*"))
      (println)))
  )



