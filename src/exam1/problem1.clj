;==========================================================
;A01374009
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================

(defn lucky-helper [n] (= (some #{7}(->> n str (map (comp read-string str))))7))
(defn lucky-number?
  "Returns true if n is a positive integer number that is
  exactly divisible by 7 or contains at least one digit 7.
  Otherwise returns false."
  [n] (or (= (mod n 7) 0) (lucky-helper n)))




   ;;; Replace nil with your code

;==========================================================
(deftest test-lucky-number?
  (is (lucky-number? 7))
  (is (lucky-number? 2828))
  (is (lucky-number? 773704))
  (is (lucky-number? 0))
  (is (lucky-number? 14890123808))
  (is (lucky-number? 100000700000))
  (is (not (lucky-number? 1)))
  (is (not (lucky-number? 26)))
  (is (not (lucky-number? 123456890)))
  (is (not (lucky-number? 14890123806))))

;==========================================================
(run-tests)