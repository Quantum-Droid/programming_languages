;----------------------------------------------------------
; Problem 61: Map Construction
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-61
  "Map construction."
  [keyss values]
  (apply hash-map (interleave keyss values)))

(deftest test-p
  (is (= (p-61 [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
  (is (= (p-61 [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
  (is (= (p-61 [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})))

(run-tests)