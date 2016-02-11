(use 'clojure.test)

(defn add-list
  [l]
  (reduce + l))

(defn list-of-symbols?
  [l]
  (every? symbol? l))

(defn invert-pairs
  [l]
  ;(map (fn [v] [(v 1) (v 0)] l))
  (map (fn [[a b]] [b a]))

  (defn insert
    [n l]
    (concat (take-while #(< % n) l)
            (l n)
            (drop-while #(<= % n) l))