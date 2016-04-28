
;; (defmacro defn-curry
;;   "It takes as parameters a name, an args vector, and a body of one or more expressions. The macro should define a
;;   function called name that takes only the first argument from args and returns a function that takes the second
;;   argument from args and returns a function that takes the third argument from args, and so on. The last function
;;   returned takes the last argument from args and evaluates all the expressions in body using a do special form."
;;   ([namen argv & body]
;;     `(defn ~namen [~(first argv)]
;;       (loop [i 1]
;;         (if (< i (count argv)))
;;          (fn [~(nth argv i)] (recur (inc i)))
;;          (do ~@body)))))