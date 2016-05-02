(defn p-156
  [keyy l]
  (loop [s {}
         l l]
    (if (empty? l)
      s
      (recur (merge s {(first l) keyy}) (rest l)))))

(p-156 "x" [1 2 3])

(defn p-25
  [l]
  (loop [res ()
         l l]
    (if (empty? l) (reverse res)
      (if (odd? (first l))
        (recur (cons (first l) res) (rest l))
        (recur res (rest l))))))

(p-25 [1 1 1 3])


(defn p-27
  [s]
  (if (string? s)
    (= s (apply str (reverse s)))
    (= s (reverse s))))

(p-27 '(a b a))

(defn p-26
  [n]
  (loop [res ()
         i 0]
    (cond
      (zero? n) res
      (> i n) (reverse res)
      (= i 0) (recur () (inc i))
      (= i 1) (recur '(1) (inc i))
      (= i 2) (recur '(1 1) (inc i))
      :else (recur (cons (+ (first res) (second res)) res) (inc i)))))

(p-26 6)

(defn p-29
  [s]
  (apply str (filter #(Character/isUpperCase %) s)))

(p-29 "LeL")

(defn p-42
  [n]
  (reduce * (take n (range 1 (inc n)))))

(defn p-28
  [l]
  (apply concat l))

(p-28 '["a" ["b"] "c"])
(p-28 '((1 2) 3 [4 [5 6]]))
