;----------------------------------------------------------
; Activity: Recursive Functions, Part I
; Date: January 28, 2016.
; Author:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn my-count
  "Returns the number of elements
  contained in its input list."
  [l]
  (if (empty? l)
    0
    (+ 1 (my-count (rest l)))))

(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

(defn add-list
  "Returns the sum of all the
  elements in its input list."
  [l]
  (if (empty? l)
    0
    (+ (first l) (add-list (rest l)))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(defn member?
  "Returns true if x is contained
  in l, false otherwise."
  [x l]
  (if (empty? l)
      false
      (if (= x (first l))
          true
          (member? x (rest l)))))

(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

(defn list-of-symbols?
  "Returns true if all the elements contained in l
  are symbols, false otherwise."
  [l]
  (if (empty? l)
    true
    (if (symbol? (first l))
      (list-of-symbols? (rest l))
      false)))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(defn my-last
  "Returns the last element of l, or
  nil if it's empty."
  [l]
  (if (empty? l)
    nil
    (if (= 1 (count l))
      (first l)
      (my-last (rest l)))))

(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

(defn cons-end
  "Takes any data x and list l, and returns l with
  x at the end."
  [x l]
  (if (empty? l) (cons x l)
    (cons (first l) (cons-end x (rest l)))))

(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

(defn my-reverse
  "Takes a list as an argument, and returns another
  list with the same elements but in reverse order."
  [l]
  (loop [l l
         res ()]
    (if (empty? l) res
      (recur (rest l) (cons (first l) res)))))

(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

(defn my-butlast
  "Returns a list with the same elements as its
  input list but excluding the las element.
  Returns nil if empty."
  [l]
  (cond
    (empty? l) nil
    (empty? (rest l)) ()
    :else (cons (first l) (my-butlast (rest l)))))

(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

(defn my-concat
  "Returns the resulting list of appending the
  two lists it takes as input."
  [A B]
  (if (empty? B) A
    (my-concat (cons-end (first B) A) (rest B))))

(defn deep-reverse
  "Returns the input list in reverse order, and also
  reverses any lists within."
  [l]
  (loop [l l
         res ()]
    (if (empty? l) res
      (if (list? (first l)) (recur (rest l) (cons (deep-reverse (first l)) res))
        (recur (rest l) (cons (first l) res))))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))

(run-tests)
