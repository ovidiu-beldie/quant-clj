; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix-utilities.core)

(defn matrix [rows cols vals]
  "Creates a matrix with the number of rows
    and columns equal to the first 2 params 
    and with values provided by the last param"
  (vec (map vec (partition cols (take (* rows cols) vals)))))

(defn column [m j]
  "Return colum j of matrix m"
  (map #(nth % j) m))

(defn assoc-column [m col j]
  "Return the matrix m with column col at index j"
    (map assoc m (repeat j) col))

(defn get-main-diag [m]
  "Returns the main diagonal of matrix m"
  (map nth m (range)))

(defn set-main-diag [m vals]
  "Sets the main diagonal of the matrix m
    with the values from vals"
  (vec (map assoc m (range) vals)))

(defn transpose [m]
  "Transposes matrix m"
  (loop [new-m [], old-m m]
    (if (empty? (first old-m))
      new-m
      (recur (conj new-m (vec (map first old-m))), (map rest old-m)))))

(defn count-rows [m]
  (count m))

(defn count-cols [m]
  (count (first m)))

(defn matrix? [m]
  "Tests if m is a matrix
   (a vector of vectors)"
  (if (vector? m)
    (vector? (first m))
    false))

(defn inner-prod [v1 v2]
  (reduce + (map * v1 v2)))

(defn multiply-matrices [x y]
  (if (not (= (count-cols x) (count-rows y)))
    (throw (IllegalArgumentException. "Matrices with different sizes cannot be multiplied"))
    (let [v (for [i x, j (transpose y)] (inner-prod i j))]
      (matrix (count-rows x) (count-cols y) v))))

(defn multiply-scalar [x y]
  (let [m (if (coll? x) x y)
        s (if (coll? x) y x)
        mul-row (fn [r]
                  (vec (map #(* s %) r)))]
    (vec (map mul-row m))))
  
(defn multiply-vector [x y]
  (let [m (if (matrix? x) x (transpose y))
        v (if (matrix? x) y x)]
    (if (not (= (count v) (count-cols m)))
      (throw (IllegalArgumentException. "Cannot multiply matrix and vector with different sizes"))
      (vec (map (partial inner-prod v) m)))))

(defn multiply-2 [x y]
  (let [l (list x y)
        matrices (map matrix? l)
        collections (map coll? l)]
    (if (not-any? true? matrices)
      (throw (IllegalArgumentException. "At least one operand must be a matrix"))
      (if (every? true? matrices)
        (multiply-matrices x y)
        (if (not-every? true? collections)
          (multiply-scalar x y)
          (multiply-vector x y))))))
                              
(defn multiply [x y & others]
  "Multiplies a matrix with other matrices,
   with vectors or with scalars. At each step, 
   at least one of the operands must be a matrix"
  (let [operands (conj others y x)]
    (reduce multiply-2 operands))) 
