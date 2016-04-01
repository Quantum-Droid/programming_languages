;----------------------------------------------------------
; Problem 118: Reimplement map
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn p-118
  "Reimplement map."
  [f s]
  (def l (if (seq? s) s (seq s)))
  (if (empty? l) ()
    (cons (f (first l)) (p-118 f (rest l)))))

(deftest test-p
  (is (= [3 4 5 6 7]
   (p-118 inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
   (p-118 (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
   (->> (p-118 (range))
        (drop (dec 1000000))
        (take 2)))))

(run-tests)