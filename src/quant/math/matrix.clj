; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

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
