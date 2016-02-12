;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.test)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(defn my-map-indexed
  "Takes two arguments: a function f and a list lst.
  It returns a list consisting of the result of applying f
  to 0 and the first item of lst, followed by applying f
  to 1 and the second item in lst, and so on until lst is
  exhausted. "
  [func l]
  (loop [l l
         i 0
         res ()]
    (if (empty? l) (reverse res)
      (recur (rest l) (inc i) (cons (func i (first l)) res)))))

(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))

(defn my-drop-while
  "Takes two arguments: a function f and a list lst. It returns
  a list of items from lst dropping the initial items that
  evaluate to true when passed to f. Once a false value is
  encountered, the rest of the list is returned."
  [func l]
  (loop [l l
         res ()]
    (if (empty? l) (reverse res)
      (if (func (first l)) (recur (rest l) res)
        (concat l res)))))

(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

(defn abs
  "Because Math/abs doesn't work."
  [n]
  (max n (- n)))

(defn bisection
  "A root-finding algorithm which works by repeatedly dividing
  an interval in half and then selecting the subinterval in
  which the root exists."
  [a b f]
  (def c (/ (+ a b) 2))
  (if (< (abs (f c)) 0.000000000000001) c
    (if (or (and (pos? (f a)) (neg? (f c))) (and (neg? (f a)) (pos? (f c)))) (bisection a c f)
      (bisection c b f))))

(deftest test-bisection
  (is (aprox= 0.0001 3.0 (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 -4.0 (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001 Math/PI (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 (* 2 Math/PI) (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001 1.618033988749895
                     (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001 -0.6180339887498948
                     (bisection -10 1 (fn [x] (- (* x x) x 1))))))

(defn deriv
  "Takes f and h as its arguments, and returns a new function
  that takes x as argument, and which represents the derivate
  of f given a certain value for h."
  [f h]
  (fn [x] (/ (- (f (+ x h)) (f x)) h)))

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

;; ===== START OF EXCERCISE 5 =====

(defn integral_yk
  "Calculating yk value for integral function."
  [f a k n h i]
  (if (or (= k 0) (= k n)) (def m 1)
    (def m (if (true? i) 4 2)))
  (* m (f (+ a (* k h)))))

(defn integral_sumY
  "Sums all the Y-values for integral function."
  [f a n h]
  (loop [k 0
         i false
         res 0]
    (if (> k n) res
      (recur (inc k) (not i) (+ res (integral_yk f a k n h i))))))

(defn integral
  "Takes as arguments a, b, n, and f. It returns the value of
  the integral, using Simpson's rule."
  [a b n f]
  (def h (/ (- b a) n))
  (* (/ h 3) (integral_sumY f a n h)))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y))))))))

; f -> function
; a -> Integral value a
; b -> Integral value b
; k -> current index on Yk
; n -> n: number of iterations of Yk (0, n)
; h -> Integral value h
; i -> boolean indicating whether it's 2 or 4
; m -> either 2 or 4

;; ===== END OF EXCERCISE 5 =====

(run-tests)
