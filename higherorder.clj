;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

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
    (if (empty? l) res
      (func (i (first l)))
