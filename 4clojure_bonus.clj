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
