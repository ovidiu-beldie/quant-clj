(ns quant.math.matrix)

(defn column [m j]
  "Return colum j of matrix m"
  (map #(nth % j) m))

(defn assoc-column [m col j]
  "Return the matrix m with column col at index j"
  (let [assoc-elem (fn [row elem]
                      (assoc row j elem))]
    (map assoc-elem m col)))
