;----------------------------------------------------------
; Problem 95: To Tree, or not to Tree
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-95
  "To Tree, or not to Tree."
  [l]
  ;(def elements (count (flatten l)))
  ;(def powers (map dec (seq (take elements (iterate #(* 2 %) 4)))))
  ;(if (fn in? [elements powers] (some #(= elements %) powers)) true false))
  (if (some false? (flatten l)) false
    (if (odd? (count (flatten l))) true false)))

(deftest test-p-95
  (is (= (p-95 '(:a (:b nil nil) nil))
   true))
  (is (= (p-95 '(:a (:b nil nil)))
   false))
  (is (= (p-95 [1 nil [2 [3 nil nil] [4 nil nil]]])
   true))
  (is (= (p-95 [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false))
  (is (= (p-95 [1 [2 [3 [4 nil nil] nil] nil] nil])
   true))
  (is (= (p-95 [1 [2 [3 [4 false nil] nil] nil] nil])
   false))
  (is (= (p-95 '(:a nil ()))
   false)))

(run-tests)
