(defn _largest
  [maxest l]
  (if (empty? l) maxest
    (if (< maxest (first l))
      (_largest (first l) (rest l))
      (_largest maxest (rest l)))))


(defn largest
  [l]
  (_largest (first l) (rest l)))

(defn decrement
  [x]
  (if (= x 0) ()
    (cons x (decrement (dec x)))))

(defn log2
  [x]
  (if (= x 1) 0
    (inc (log2 (quot x 2)))))

(largest '(1 2 3 4))
(decrement 5)
(log2 1024)