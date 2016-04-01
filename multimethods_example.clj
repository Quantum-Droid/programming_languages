(defn my-println
  [obj]
  (my-print obj)
  (.write *out* "\n"))

(defmulti my-print class)

(defmethod my-print String
  [s]
  (.write *out* s))

(defmethod my-print nil
  [s]
  (.write *out* "nil"))

(defmethod my-print Number
  [n]
  (.write *out* (.toString n)))

(defmethod my-print java.util.Collection
  [c]
  (.write *out* "(")
  (.write *out* (str/join " " c))
  (.write *out* ")"))

(defmethod my-print :default
  [obj]
  (.write *out* (.toString obj)))