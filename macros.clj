;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

; PROTIP -> use (macroexpand-1 '(my-macro a b c _)) to see the macro expansion.

(defmacro my-and
  ([] true)
  ([x] x)
  ([x & y]
   `(let [temp# ~x]
      (if temp#
        (my-and ~@y)
        temp#))))

(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [temp# ~x]
      (if temp#
        temp#
        (my-or ~@next)))))

(my-or)
(my-or false :one nil :two false :three)
(my-or false false nil)
(my-or nil nil false)

(defmacro debug [e]
  `(let [temp# ~e]
    (do
      (printf "debug: %s => %s%n" '~e temp#)
      temp#)))

(inc (debug (* 4 5)))