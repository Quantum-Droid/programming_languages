;----------------------------------------------------------
; Problem 83: A Half-Truth
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-83
  "A Half-Truth."
  [& l]
  (def j (and (some true? l) (some false? l)))
  (if (nil? j) false
    j))

(deftest test-p-83
  (is (= false (p-83 false false)))
  (is (= true (p-83 true false)))
  (is (= false (p-83 true)))
  (is (= true (p-83 false true false)))
  (is (= false (p-83 true true true)))
  (is (= true (p-83 true true true false))))

(run-tests)