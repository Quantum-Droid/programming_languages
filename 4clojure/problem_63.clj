;----------------------------------------------------------
; Problem 63: Group a Sequence
; Date: March 17, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;
;          NOTA: Algunos resultados salen en desorden,
;                por lo que los tests salen fallidos.
;----------------------------------------------------------

(use 'clojure.test)

(defn p-63
  "Group a Sequence."
  [f v]
  (def l (if (seq? v) v (seq v)))
  (def valuess (partition-by f l))
  (def values (loop [valuess valuess
                     res ()]
                (if (empty? valuess) (reverse res)
                  (recur (rest valuess) (cons (vec (first valuess)) res)))))
  (def keyss (distinct (loop [values values
                    res ()]
               (if (empty? values) (reverse res)
                 (recur (rest values) (cons (f (first (first values))) res))))))
  (apply hash-map (interleave keyss values)))

(deftest test-p-63
  (is (= (p-63 #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (p-63 #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
  (is (= (p-63 count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})))

(p-63 #(> % 5) [1 3 6 8])

(run-tests)