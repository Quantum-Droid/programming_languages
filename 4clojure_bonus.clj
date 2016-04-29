(defn p-156
  [keyy l]
  (loop [s {}
         l l]
    (if (empty? l)
      s
      (recur (merge s {(first l) keyy}) (rest l)))))

(p-156 "x" [1 2 3])
