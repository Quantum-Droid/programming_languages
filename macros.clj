;----------------------------------------------------------
; Activity: Higher-Order Functions
; Date: February 11, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

; PROTIP -> use (macroexpand-1 '(my-macro a b c _)) to see the macro expansion.

(defmacro my-and
  "Implements and from Clojure."
  ([] true)
  ([x] x)
  ([x & y]
   `(let [temp# ~x]
      (if temp#
        (my-and ~@y)
        temp#))))

(defmacro my-or
  "Implements or from Clojure."
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [temp# ~x]
      (if temp#
        temp#
        (my-or ~@next)))))

(my-or)
(macroexpand-1 '(my-or false :one nil :two false :three))
(my-or false false nil)
(my-or nil nil false)

(defmacro do-loop
  "A post-test loop that implements the do-while from C and the
  repeat-until from Pascal"
  ([& y]
   `(let [condition# ~(first (last y))
          statement# ~(second (last y))]
      (do
        ; Execute body
;;         (loop [i# 0
;;                y# ~(butlast y)]
;;           (eval (nth ~y))
;;           (if (< i# (count y#))
;;             (recur (inc i#) y#)
;;             nil))
        ; Check for condition
        (cond
          (= condition# :while) (if statement#
;;                                   (do-loop ~@y)
                                  "while"
                                  "while nil")
          (= condition# :until) (if (not statement#)
;;                                   (do-loop ~@y)
                                  "until"
                                  "until nil"))))))

(def i (atom 0))
(macroexpand-1 '(do-loop (println @i) (swap! i inc) (:until (= @i 5))))
(do-loop (println @i) (swap! i inc) (:until (= @i 5)))

(macroexpand-1 '(do-loop (println @i) (swap! i inc) (:until (= 5 5))))
(do-loop (println @i) (swap! i inc) (:until (= 5 5)))

(defn thing
  [a b & y]
  (count y))

(thing 'a 'b 'c)

;; (defmacro debug [e]
;;   `(let [temp# ~e]
;;     (do
;;       (printf "debug: %s => %s%n" '~e temp#)
;;       temp#)))
