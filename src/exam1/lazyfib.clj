(defn fibs 
  ([]
   (fibs 0 1))
  ([a b] (lazy-seq (cons a (fibs b (+' a b))))))