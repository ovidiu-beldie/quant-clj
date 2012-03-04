(ns quant.math.matrix)

(defn matrix [rows cols vals]
  "Creates a matrix with the number of rows
    and columns equal to the first 2 params 
    and with values provided by the last param"
  (vec (map vec (partition cols (take (* rows cols) vals)))))

(defn column [m j]
  "Return colum j of matrix m"
  (map #(nth % j) m))

(defn assoc-elem [row elem i]
  "Returns row with elem replacing
    the element at index i"
  (assoc row i elem))

(defn assoc-column [m col j]
  "Return the matrix m with column col at index j"
    (map assoc-elem m col (repeat j)))

(defn set-main-diag [m vals]
  "Sets the main diagonal of the matrix m
    with the values from vals"
  (map assoc-elem m vals (range)))

