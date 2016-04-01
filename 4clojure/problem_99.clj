;----------------------------------------------------------
; Problem 60: Sequence reductions
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-99
  "Multiplies two numbers and returns the result as a sequence of its digits."
  [a b]
  (map #(- (int %) (int \0)) (str (* a b))))

(deftest test-p-99
  (is (= (p-99 1 1) [1]))
  (is (= (p-99 99 9) [8 9 1]))
  (is (= (p-99 999 99) [9 8 9 0 1])))

(run-tests)