;==========================================================
; Diego Monroy Fraustro  A01165792
;==========================================================

(use 'clojure.test)

;==========================================================
(defn get-cost-per-sqIn
  "Calculates the cost per sq inch for a pizza vector => [diam cost]."
  [pizza]
  (def radius (/ (first pizza) 2))
  (/ (second pizza) (* Math/PI radius radius)))

(defn pizza
  "Takes a list of vectors lst. Each vector contains two
  integers respectively designating a pizza's diameter d
  (in inches) and price p (in dollars). It returns the
  diameter d of the pizza with the best value."
  [l]
  (first (reduce (fn f [a b] (cond
                    (< (get-cost-per-sqIn a) (get-cost-per-sqIn b)) a
                    (< (get-cost-per-sqIn b) (get-cost-per-sqIn a)) b
                    :else a)) l)))

;==========================================================
(defrecord State [state-name abbreviation])

(def s1 (State. "California" :CA))
(def s2 (State. "Oregon" :OR))
(def s3 (State. "Texas" :TX))

(defrecord Item [price])
(defrecord Gasoline [price-per-gallon gallons])
(defrecord Cigarettes [price])

(def i (Item. 9.99))
(def g (Gasoline. 3.75 15))
(def c (Cigarettes. 4.50))

(defn getSalesTax
  "Gets the appropiate value for Item tax."
  [state]
  (cond
    (= :CA (.abbreviation state)) 1.0825
    (= :OR (.abbreviation state)) 1.0
    (= :TX (.abbreviation state)) 1.0625))

(defn getGasolineTax
  "Gets the appropiate value for Gasoline tax."
  [state]
  (cond
    (= :CA (.abbreviation state)) 0.46
    (= :OR (.abbreviation state)) 0.25
    (= :TX (.abbreviation state)) 0.20))

(defn getCigaretteTax
  "Gets the appropiate value for Cigarettes tax."
  [state]
  (cond
    (= :CA (.abbreviation state)) 0.87
    (= :OR (.abbreviation state)) 1.18
    (= :TX (.abbreviation state)) 1.41))

(defmulti total
  "Multimethod that allows you to compute the total price,
  including tax, of a certain article in a certain state."
  (fn [article state]
    (cond
      (= article i) :Item
      (= article g) :Gasoline
      (= article c) :Cigarettes)))

(defmethod total :Item
  [article state]
  (* (.price article) (getSalesTax state)))

(defmethod total :Gasoline
  [article state]
  (* (.gallons article) (+ (.price-per-gallon article) (getGasolineTax state))))

(defmethod total :Cigarettes
  [article state]
  (+ (.price article) (getCigaretteTax state)))

(defmethod total :default
  [article state]
  nil)

(total g s1)
(total c s2)

;==========================================================
(deftest test-pizza
  (is (= 12 (pizza '([5 2] [10 6] [12 8]))))
  (is (= 10 (pizza '([5 2] [10 5] [12 8]))))
  (is (= 24 (pizza '([1 1] [24 33] [13 11] [6 11])))))

;==========================================================

(deftest test-total
  (is (= 10.814175 (total i s1)))
  (is (= 9.99 (total i s2)))
  (is (= 10.614375 (total i s3)))
  (is (= 63.15 (total g s1)))
  (is (= 60.0 (total g s2)))
  (is (= 59.25 (total g s3)))
  (is (= 5.37 (total c s1)))
  (is (= 5.68 (total c s2)))
  (is (= 5.91 (total c s3))))

;==========================================================
(run-tests)
