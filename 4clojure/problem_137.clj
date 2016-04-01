;----------------------------------------------------------
; Problem 137: Digits and Bases
; Date: march 17, 2016.
; Author:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(def p-137
  "Digits and Bases."
  (fn [x y]
    (loop [a x
           lst []]
      (cond
        (zero? a) (if (empty? lst) [0] (reverse lst))
        :else (recur (quot a y) (conj lst (mod a y)))))))

(deftest test-p
    (= [1 2 3 4 5 0 1] (p-137 1234501 10))
    (= [0] (p-137 0 11))
    (= [1 0 0 1] (p-137 9 2))
    (= [1 0] (let [n (rand-int 100000)](p-137 n n)))
    (= [16 18 5 24 15 1] (p-137 Integer/MAX_VALUE 42))
)

(run-tests)