;----------------------------------------------------------
; Activity: Macros
; Date: April 14th, 2016.
; Authors:
;          A01165792 Diego Monroy Fraustro
;----------------------------------------------------------

; PROTIP -> use (macroexpand-1 '(my-macro a b c _)) to see the macro expansion.

;; ==== my-and (example from lecture)

(defmacro my-and
  "Implements and from Clojure."
  ([] true)
  ([x] x)
  ([x & y]
   `(let [temp# ~x]
      (if temp#
        (my-and ~@y)
        temp#))))

;; ======== my-or

(defmacro my-or
  "Implements or from Clojure."
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [temp# ~x]
      (if temp#
        temp#
        (my-or ~@next)))))

;; test cases

(my-or)
(macroexpand-1 '(my-or false :one nil :two false :three))
(my-or false false nil)
(my-or nil nil false)

;; ======== do-loop

(defmacro do-loop
  "A post-test loop that implements the do-while from C and the
  repeat-until from Pascal."
  ([& y]
   (loop []
     `(let [condition# ~(first (last y))
            statement# ~(second (last y))]
          (cond
            (= condition# :while) (if statement#
                                    (do
                                      ~@(butlast y)
                                      (recur ))
                                    nil)
            (= condition# :until) (if (not statement#)
                                    (do
                                      ~@(butlast y)
                                      (recur ))
                                    nil))))))

;; test cases

(def i (atom 0))
(do-loop (println @i) (swap! i inc) (:until (= @i 5)))
(macroexpand-1 '(do-loop (println @i) (swap! i inc) (:until (= @i 5))))

(def j (atom 1))
(do-loop (println @j) (swap! j inc) (:while (<= @j 5)))

;; ======== def-pred

(defmacro def-pred
  "Takes a name, an arg vector, and a body of one or more expressions. The macro should define two predicate
  functions: a regular one and its negated version. The name of the negated predicate should be the same as
  name but with a not- prefix, and its result should be negated using the not function."
  ([namen argv & body]
   `(do
     (defn ~namen ~argv ~@body)
     (defn ~(symbol (clojure.string/join "" ["not-" namen])) ~argv (not (do ~@body))))))

;; test cases

(macroexpand-1 '(def-pred less-than-one? [x] (< x 1)))
(def-pred less-than-one? [x] (< x 1))

(less-than-one? 0)
(less-than-one? 2)
(not-less-than-one? 0)
(not-less-than-one? 2)

(macroexpand-1 '(def-pred plural? [s] (println "check s in" s) (= \s (last s))))
(def-pred plural? [s] (println "check s in" s) (= \s (last s)))

(plural? "boys")
(plural? "girl")
(not-plural? "boys")
(not-plural? "girl")

;; ======== defn-curry

(defmacro defn-curry [namen argv & body]
  "Performs a currying transformation to a function defined as name
  that takes the first argument of args and recursively returns a
  function taking the rest of args. The last function returned executes body on all args"
  (if (empty? argv)
    `(defn ~namen ~argv ~@body)
    (let [gen-funcs (reduce (fn [a1 a2] `(letfn [(new-fun#
                                                   ([] new-fun#)
                                                   ([x#] (let [~a2 x#] ~a1))
                                                   ([x# & rest#] (apply (new-fun# x#) rest#)))]
                                           new-fun#))
                            `(do ~@body) (reverse argv))]
      `(defn ~namen [& argv#]
           (apply ~gen-funcs argv#)))))


(macroexpand-1 '(defn-curry sum [a b c d] (prn 'args a b c d) (+ a b c d)))
(defn-curry sum [a b c d] (prn 'args a b c d) (+ a b c d))
((((sum 1) 2) 3) 4)
((((sum 15) 8) 16) 42)

(macroexpand-1 '(defn-curry go [x y] (* x (+ y 1))))
(defn-curry go [x y] (* x (+ y 1)))
((go 2) 3)
((go 3) 2)

(macroexpand-1 '(defn-curry add1 [x] (+ x 1)))
(defn-curry add1 [x] (+ x 1))
(add1 0)
(add1 41)

(macroexpand-1 '(defn-curry hello [] "hello"))
(defn-curry hello [] "hello")
(hello)

;; (defmacro debug [e]
;;   `(let [temp# ~e]
;;     (do
;;       (printf "debug: %s => %s%n" '~e temp#)
;;       temp#)))
