;----------------------------------------------------------
; Activity: Logic programming with MiniKanren
; Date: May 2nd, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

(use 'clojure.core.logic)
; (refer-clojure :exclude '[logic-is])
; (require '[clojure.core :as core])
(use '[clojure.core.logic :rename {is logic-is}])
(use 'clojure.test)

;;                  \
;; ================= \
;; ===================> NOTE: Couldn't run tests because of weird compilation error when importing clojure.test (even when renaming):
;; ================= /                              "clojure.lang.Compiler$CompilerException: java.lang.IllegalStateException: is already refers to: #'clojure.core.logic/is in namespace: user, 
;;                  /                                compiling:(/home/ubuntu/workspace/Homework/logic.clj:12:1)"
;;                            ...So I hope this runs with the actual tests, other than that answers should be correct.

; This logic function succeeds if it’s able to remove the first occurrence of x from lst giving result.

(defne removeo
  [x l result]
  ([x [x . t] result] (== result t))
  ([x [h . t] result]
    (fresh [prince]
      (removeo x t prince)
      (conso h prince result))))
    
; These two mutually recursive logic functions succeed if the number of elements in lst is even or odd, respectively.

(declare odd-sizeo even-sizeo)

(defne odd-sizeo 
  [l]
  ([[_ . t]] (even-sizeo t)))

(defne even-sizeo 
  [l]
  ([[]])
  ([[_ . t]] (odd-sizeo t)))

; This logic function succeeds if lst is a palindrome list (it reads the same from left to right than from right to left). 

(defne lasto
  [l n mid]
  ([[n] n mid] (== mid []))
  ([[_] n mid] fail)
  ([[h . t] n mid]
   (fresh [prince]
    (lasto t n prince)
    (conso h prince mid))))
   
(defne palindromeo
  [l]
  ([[]])
  ([[_]])
  ([[h . t]]
    (fresh [prince]
      (lasto t h prince)
      (palindromeo prince))))
  
; This logic function succeeds when lst is rotated left one position giving result. In other words, the first element of lst becomes the last element of result.

(defne but-firsto-r
  [l but-first firstt]
  ([[] [] []])
  ([[x] [] x])
  ([[h . t] t h]))

(defne but-lasto-r
  [l but-last lastt]
  ([[] [] []])
  ([[x] [] x])
  ([[h . t] but-last lastt]
    (fresh [prince of]
      (== lastt of)
      (but-lasto-r t prince of)
      (conso h prince but-last))))

(defne rotateo
  [l result]
  ([[] []])
  ([[x] [x]])
  ([[h . t] result]
    (fresh [prince of bel]
      (but-lasto-r result prince of)
      (== h of))))
    
; This logic function succeeds when digit d corresponds to the keyword k (for example digit 7 with keyword :seven).

(defne converto
  [d k]
  ([0 :zero])
  ([1 :one])
  ([2 :two])
  ([3 :three])
  ([4 :four])
  ([5 :five])
  ([6 :six])
  ([7 :seven])
  ([8 :eight])
  ([9 :nine]))

;This logic function succeeds when all digits contained in lst are converted to their corresponding keywords (using the converto logic function from the previous problem) giving result.

(defne translateo
  [l result]
  ([[] []])
  ([[x] y]
    (converto x y))
  ([[h . t] result]
    (fresh [prince of]
      (converto h prince)
      (translateo t of)
      (conso prince of result))))

; This logic function succeeds when splitting lst gives a and b. The first, third, fifth, etc. elements of lst go to a, while the second, fourth, sixth, etc. elements go to b.

(defne splito
  [l a b]
  ([[] [] []])
  ([[x] [x] []])
  ([[x y . t] a b]
    (fresh [prince of]
      (splito t prince of)
      (conso x prince a)
      (conso y of b))))
    
;; ======== BONUS ========

; Bonus function that reverses a string in a wierd way. Done because I misunderstood rotateo instructions. Still, check out those FRESH variables!

(defne reverseo-bonus
  [l result]
  ([[] []])
  ([l result]
    (fresh [prince of bel air]
      (but-firsto-r l prince of)
      (but-lasto-r result bel air)
      (== of air)
      (reverseo-bonus prince bel))))

;; ======== TESTS ========

(deftest test-removeo
  (is (= [[:b :c :d :e]]
        (run 1 [q] (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
        (run 1 [q] (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
        (run 1 [q] (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
        (run 1 [q] (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e] 
          [:a :b :x :c :d :e] 
          [:a :b :c :x :d :e] 
          [:a :b :c :d :x :e] 
          [:a :b :c :d :e :x]]
        (run 6 [q] (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]] 
          [:b [:a :c :d :e]] 
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
        (run* [q1 q2] 
          (removeo q1 [:a :b :c :d :e] q2)))))

(deftest test-even-sizeo-odd-sizeo
  (is (= [:yes]
        (run 1 [q] (even-sizeo []) (== q :yes))))
  (is (= [:yes]
        (run 1 [q] (odd-sizeo [:x]) (== q :yes))))
  (is (= []
        (run 1 [q] (even-sizeo [:x]) (== q :yes))))
  (is (= []
        (run 1 [q] (odd-sizeo []) (== q :yes))))
  (is (= [:yes]
        (run 1 [q] (even-sizeo [:a :b :c :d :e :f]) (== q :yes))))
  (is (= [:yes]
        (run 1 [q] (odd-sizeo [:a :b :c :d :e]) (== q :yes))))
  (is (= '[[] 
          [_0 _1]
          [_0 _1 _2 _3]
          [_0 _1 _2 _3 _4 _5] 
          [_0 _1 _2 _3 _4 _5 _6 _7]]
        (run 5 [q] (even-sizeo q))))
  (is (= '[[_0] 
          [_0 _1 _2]
          [_0 _1 _2 _3 _4]
          [_0 _1 _2 _3 _4 _5 _6] 
          [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
        (run 5 [q] (odd-sizeo q)))))

(deftest test-palindromeo
  (is (= [:yes]
        (run 1 [q] (palindromeo []) (== q :yes))))
  (is (= [:yes]
        (run 1 [q] (palindromeo [:a]) (== q :yes))))
  (is (= [:yes]
        (run 1 [q] (palindromeo [:a :b :c :b :a]) (== q :yes))))
  (is (= []
        (run 1 [q] (palindromeo [:a :b :c :d]) (== q :yes))))
  (is (= '[[] 
          [_0]
          [_0 _0]
          [_0 _1 _0] 
          [_0 _1 _1 _0] 
          [_0 _1 _2 _1 _0] 
          [_0 _1 _2 _2 _1 _0]]
        (run 7 [q] (palindromeo q))))

(deftest test-rotateo
  (is (= [:yes]
        (run 1 [q] 
          (rotateo [:a :b :c :d :e] 
                    [:b :c :d :e :a])
          (== q :yes))))
  (is (= []
        (run 1 [q] 
          (rotateo [:a :b :c :d :e] 
                    [:a :b :c :d :e])
          (== q :yes))))
  (is (= []
        (run 1 [q] (rotateo [] q))))
  (is (= [[:a]]
        (run 1 [q] (rotateo [:a] q))))
  (is (= [[:b :c :d :e :a]]
        (run 1 [q] (rotateo [:a :b :c :d :e] q))))
  (is (= [[:e :a :b :c :d]]
        (run 1 [q] (rotateo q [:a :b :c :d :e]))))
  (is (= '[[[_0] [_0]]
          [[_0 _1] [_1 _0]]
          [[_0 _1 _2] [_1 _2 _0]]
          [[_0 _1 _2 _3] [_1 _2 _3 _0]] 
          [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
          [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
          [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
        (run 7 [q1 q2] (rotateo q1 q2)))))

(deftest test-converto
  (is (= [:yes]
        (run 1 [q] (converto 0 :zero) (== q :yes))))  
  (is (= [:yes]
        (run 1 [q] 
          (converto 0 :zero)
          (converto 1 :one)
          (converto 2 :two)
          (converto 3 :three)
          (converto 4 :four)
          (converto 5 :five)
          (converto 6 :six)
          (converto 7 :seven)
          (converto 8 :eight)
          (converto 9 :nine)
          (== q :yes))))
  (is (= []
        (run 1 [q] (converto 2 :one) (== q :yes))))
  (is (= []
        (run 1 [q] (converto 12 :twelve) (== q :yes))))         
  (is (= [7]
        (run 1 [q] (converto q :seven))))
  (is (= [:seven]
        (run 1 [q] (converto 7 q))))
  (is (= [[1 :two 3]]
        (run 1 [q1 q2 q3] 
          (converto q1 :one)
          (converto 2 q2)
          (converto q3 :three)))))

(deftest test-translateo
  (is (= [:yes]
        (run 1 [q] (translateo [1 2 3] [:one :two :three]) (== q :yes))))
  (is (= []
        (run 1 [q] (translateo [1 2 3] [:one :two :four]) (== q :yes))))
  (is (= [:three]
        (run 1 [q] (translateo [1 2 3] [:one :two q]))))
  (is (= [[:four :five :six :seven :eight :nine]]
        (run 1 [q] (translateo [4 5 6 7 8 9] q))))
  (is (= [[1 2 0]]
        (run 1 [q] (translateo q [:one :two :zero]))))
  (is (= [[[] []]]
        (run 1 [q1 q2] (translateo q1 q2)))))

(run-tests)