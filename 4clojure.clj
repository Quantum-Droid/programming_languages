(defn nth-element
  [s n]
  (if (zero? n) (first s)
    (f (rest s) (dec n))))

(defn implement-range
  [a b]
  (take (- b a) (iterate #(+ % 1) a)))

(defn maximum-value
  [& l]
  (reduce (fn f [a b] (cond
                    (> a b) a
                    (> b a) b
                    :else a)) l))

(defn comparisons
  [& l]
  (cond
    ((first l) (second l) (last l)) :lt
    ((first l) (last l) (second l)) :gt
    :else :eq))

(defn set-intersections
  [a b]
  (loop [a a
         res {}]
    (if (empty? a) res
      (recur (rest a) (union (filter (contains? (first a) b)))))