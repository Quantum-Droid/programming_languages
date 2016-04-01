;----------------------------------------------------------
; Problem 60: Sequence Reductions
; Date: March 17, 2016.
; Author:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(def p-60
  "Sequence Reductions."
  (fn fun
    ([f value]
     (fun f (first value) (rest value)))
    ([f value c]
     (if (empty? c)
       (list value)
       (lazy-seq (cons value (fun f (f value (first c)) (rest c))))))))

(deftest test-p-60
    (= (take 5 (p-60 + (range))) [0 1 3 6 10])
    (= (p-60 conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
    (= (last (p-60 * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
)

(run-tests)