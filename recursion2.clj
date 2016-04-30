;----------------------------------------------------------
; Activity: Recursive Functions, Part II
; Date: February 4th, 2016.
; Author:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

; ============> PRIME-FACTORS DOES NOT WORK, DIDN'T
;               FIND BUG ON OUT OF BOUNDS EXCEPTION :(

(use 'clojure.test)

(defn my-repeat
  "Takes a number n and any data x. It returns a list of
  n copies of x."
  [n x]
  (if (zero? n) ()
    (cons x (my-repeat (dec n) x))))

(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

(defn invert-pairs
  "Takes a list of vectors contaning two elements each,
  and returns the list of pairs inverted."
  [l]
  (if (empty? l) ()
    (cons (rseq (first l)) (invert-pairs (rest l)))))

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(defn enlist
  "Surrounds in a list every upper-level element of the list
  it takes as input."
  [l]
  (if (empty? l) ()
    (cons (cons (first l) ()) (enlist (rest l)))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

(defn my-interleave
  "Takes two arguments: the lists a and b. It returns a list
  containing the first element of a, followed by the first
  element of b, followed by the second element of a, followed
  by the second element of b, and so on."
  [A B]
  (reverse A)
  (reverse B)
  (if (or (empty? A) (empty? B)) ()
    (conj (my-interleave (rest A) (rest B)) (first B) (first A))))

(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5) (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))


(defn my-flatten
  "Removes all the interior parenthesis of the list it takes
  as input."
  [l]
  (loop [l l
         res ()]
    (if (empty? l) (reverse res)
      (if (list? (first l)) (recur (rest l) (concat (reverse (my-flatten (first l))) res))
        (recur (rest l) (cons (first l) res))))))

(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

(defn exchange
  "Takes three arguments: two non-list values x1 and x2, and a list lst. It returns
  a list with the same elements as lst, except that all occurrences of x1 are
  replaced by x2 and vice versa, including any occurrences inside nested lists."
  [x1 x2 l]
  (loop [l l
         res ()]
    (if (empty? l) (reverse res)
      (if (list? (first l)) (recur (rest l) (concat (exchange x1 x2 (rest l)) res))
        (recur (rest l) (cons (cond
                                (= (first l) x1) x2
                                (= (first l) x2) x1
                                :else (first l)) res))))))

(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
         (exchange true 42 '((true) 42 ((cool (42)) (true))))))

(defn insert
  "takes two arguments: a number n and a list of numbers lst in
  ascending order. It returns a new list with the same elements
  as lst but inserting n in its corresponding place."
  [x l]
  (if (empty? l) (cons x ())
    (if (<= x (first l)) (cons x l)
      (conj (insert x (rest l)) (first l)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))


(defn my-sort
  "Takes an unordered list of numbers as an argument, and returns
  a new list with the same elements but in ascending order."
  [l]
  (loop [l l
         res ()]
    (if (empty? l) res
      (recur (rest l) (insert (first l) res)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(defn binary
  "takes an integer n as input (assume that n ≥ 0). If n is equal
  to zero, it returns an empty list. If n is greater than zero,
  it returns a list with a sequence of ones and zeros equivalent
  to the binary representation of n."
  [x]
  (if (zero? x) ()
    (if (= x 1) (cons 1 ())
      (reverse (cons (mod x 2) (reverse (binary (int (/ x 2)))))))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(defn next-prime
  "Calculates next prime."
  [x]
  (let [primes '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101)]
  (loop [x x
         i 0]
    (if (= (nth primes i) x) (nth primes (inc i))
      (recur x (inc i))))))

(defn prime-factors
  "Takes an integer n as input (assume that n > 0), and returns
  a list containing the prime factors of n in ascending order.
  The prime factors are the prime numbers that divide a number
  exactly."
  [x]
  (if (= x 1) ()
    (loop [x x
           p 2
           res ()]
      (if (= (- x p) 0) (cons p res)
        (if (= (rem x p) 0) (recur (/ x p) p (cons p (prime-factors (/ x p))))
          (recur (/ x (next-prime p)) (next-prime p) res))))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(defn compress
  "Takes a list lst as its argument. If lst contains consecutive repeated
  elements, they should be replaced with a single copy of the element."
  [l]
  (loop [l l
         res ()]
    (if (empty? l) (reverse res)
      (if (= (first res) (first l)) (recur (rest l) res)
        (recur (rest l) (cons (first l) res))))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(defn pack
  "If lst contains consecutive repeated elements they should be
  placed in separate sublists."
  [l]
  (loop [l l
         packed ()
         res ()]
    (cond
      (empty? l) (reverse (concat res packed))
      (= (first l) (second l)) (recur (rest l) packed (cons (first l) res))
      :else (recur (rest l) (cons (cons (first l) res) packed)()))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of elements
  in lst are encoded as vectors [n e], where n is the number of
  duplicates of the element e."
  [l]
  (loop [l l
         new ()
         enc ()]
    (cond
     (empty? l) (reverse new)
     (= (first l) (second  l)) (recur (rest l) new (cons (first l) enc))
     :else (recur (rest l) (cons (vector (count (cons (first l) enc)) (first l)) new) ()))))

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(defn encode-modified
  "Takes a list lst as its argument. It works the same as the previous
  problem, but if an element has no duplicates it is simply copied into
  the result list. Only elements with duplicates are converted to [n e]
  vectors."
  [l]
  (loop [l l
         new ()
         enc ()]
    (cond
     (empty? l) (reverse new)
     (= (first l) (second l)) (recur (rest l) new (cons (first l) enc))
     (= (count (cons (first l) enc)) 1)
     (recur (rest l) (cons (first l) new) ())
     :else (recur (rest l)
                  (cons (vector (count (cons (first l) enc)) (first l)) new)
                  ()))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(defn conj-end
  "Returns the list with x at the end."
  [l x]
  (if (empty? l) (cons x l)
    (cons (first l) (conj-end (rest l) x))))

(defn decode
  "Takes as its argument an encoded list lst that has the same structure
  as the resulting list from the previous problem. It returns the decoded
  version of lst."
  [l]
  (loop [l l
         new ()]
    (cond
     (empty? l) new
     (vector? (first l)) (recur (rest l)
                                (concat new (my-repeat (first (first l)) (second (first l)))))
     :else (recur (rest l)
                  (conj-end new (first l))))))

(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
