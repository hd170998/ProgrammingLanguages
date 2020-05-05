

(defn compute_pi
  [num_rects]
  (let [width (/ 1 num_rects)]
    (loop [sum 0.0
           i 0]
      (let [mid (* (+ i 0.5) width)
            height (/ 4.0 (+ 1.0 (* mid mid)))]
        (if (= i num_rects)
          (* width sum)
          (recur (+ sum height) (inc i)))))))






