(defn my-reduce
  [fun init l]
  (if (empty? l) init
    (fun (first l)
         (my-reduce fun init (rest l)))))

