; Problem  #2
; @author A01374009 Hector David Hernandez Rodriguez
; @author A01372942 Juan Manuel Gomez Portugal
;----------------------------------------------------------


(defn conway-helper
  [lst]
  (mapcat #(list(count %) (first %)) (partition-by identity lst)))


(defn conway
  ([]
   (conway '(1)))
  ([a] (lazy-seq (cons a (conway (conway-helper a))))))