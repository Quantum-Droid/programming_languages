;----------------------------------------------------------
; Problem 135: Infix calculator
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-135
  "Infix calculator."
  [& l]
  (loop [li (rest l)
         res (first l)]
    (if (empty? li) res
      (recur (rest (rest li)) ((first li) res (second li))))))

(p-135 1 + 2)

(deftest test-p-135
  (is (= 7  (p-135 2 + 5)))
  (is (= 42 (p-135 38 + 48 - 2 / 2)))
  (is (= 8  (p-135 10 / 2 - 1 * 2)))
  (is (= 72 (p-135 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))))

(run-tests)