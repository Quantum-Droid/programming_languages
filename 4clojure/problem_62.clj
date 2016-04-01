;----------------------------------------------------------
; Problem 62: Reimplement iterate
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-62
  "Reimplement iterate."
  [f x]
  (lazy-seq (cons x (p-62 f (f x)))))

(deftest test-p
  (is (= (take 5 (p-62 #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (p-62 inc 0)) (take 100 (range))))
  (is (= (take 9 (p-62 #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(run-tests)
