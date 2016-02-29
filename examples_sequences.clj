(use 'clojure.test)

(defn add-list
  [l]
  (reduce + l))

(defn list-of-symbols?
  [l]
  (every? symbol? l))

(defn invert-pairs
  [l]
  ;(map (fn [v] [(v 1) (v 0)] l))
  (map (fn [[a b]] [b a])))

(defn insert
  [n l]
  (let [f #(< % n)]
    (concat (take-while f l)
            (l n)
            (drop-while f l))))

(defn binary
  [n]
  (second
    (first
      (drop-while
        (fn [[n _]] (not (zero? n)))
        (iterate (fn [[n r]]
                   [(quot n 2) (cons (rem n 2) r)])
                 [n ()])))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

;; (deftest test-insert
;;   (is (= '(14) (insert 14 ())))
;;   (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
;;   (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
;;   (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(run-tests)
