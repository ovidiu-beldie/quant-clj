; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix-utilities.orthogonal-projections
  (:use [quant.math.matrix :only (matrix transpose count-rows count-cols)]
        [incanter.core :only (sqrt abs)]))

(defstruct orthog-proj-args :orig-vecs :ortho-norm-vecs :valid-vecs :tolerance :multiplier-cutoff :curr-vec)
(defstruct orthogonal-projections :projected-vecs :valid-vecs)


(defn inner-product [m1 r1 m2 r2]
  (reduce + (map * (m1 r1) (m2 r2))))

(defn norm-squared [m r]
  (inner-product m r m r))

(defn norm [m r]
  (sqrt (norm-squared m r)))


(defn update-ortho-norm-vecs [orig-vecs ortho-norm-vecs valid-vecs k j]
  (let [pred (fn [l] (and (valid-vecs l) (not (= l j))))
        dot-prods (for [l (range k) :when pred] (inner-product ortho-norm-vecs k ortho-norm-vecs l))
        ordered-onv (map vector ortho-norm-vecs (range k))
        onv-filtered (for [onv ordered-onv :when (pred (second onv))] (first onv))
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (reduce init-onv-elem - (map * dot-prods onv-filtered-col)))]
    (assoc ortho-norm-vecs k (map comp-onv-elem (ortho-norm-vecs k) (transpose onv-filtered)))))


(defn ortho-normal-basis-iter [{:keys [orig-vecs ortho-norm-vecs valid-vecs tolerance]} k j]
  (let [onv (assoc ortho-norm-vecs k (orig-vecs k))]
    (if (and (not (= k j)) (valid-vecs k))
      (let [upd-onv (update-ortho-norm-vecs orig-vecs onv valid-vecs k j)
            norm-before-scaling (norm upd-onv k)]
        (if (< norm-before-scaling tolerance)
          {:valid-vecs (assoc valid-vecs k false) :onv upd-onv}
          (let [norm-before-scaling-recip (/ 1 norm-before-scaling)
                row-k (map #(* % norm-before-scaling-recip) (upd-onv k))]
            {:valid-vecs valid-vecs, :onv (assoc upd-onv k row-k)})))
      {:valid-vecs valid-vecs, :onv ortho-norm-vecs})))
      
(defn ortho-normal-basis-loop [{:keys [orig-vecs valid-vecs ortho-norm-vecs]} j]
  (let [num-vecs (count-rows orig-vecs)]
    (loop [k 0, vecs {:valid-vecs valid-vecs, :onv ortho-norm-vecs}]
      (if (= k num-vecs)
        vecs
        (recur (inc k) (ortho-normal-basis-iter orig-vecs (vecs :valid-vecs) (vecs :onv) k j))))))

(defn handle-valid-vec [orig-vecs {:keys [orig-vecs ortho-norm-vecs valid-vecs curr-vec multiplier-cutoff] :as arg} j]
  (let [res (ortho-normal-basis-loop arg j)
        onv (update-ortho-norm-vecs orig-vecs (res :onv) (res :valid-vecs) (count-rows orig-vecs) j)
        proj-on-orig-dir (inner-product orig-vecs j onv j)
        size-muliplier (/ (norm-squared orig-vecs j) proj-on-orig-dir)]
    (if (< (abs size-muliplier) multiplier-cutoff)
      (assoc arg :ortho-norm-vecs onv, :valid-vecs valid-vecs, :curr-vec (map #(* % size-muliplier) (onv j)))
      (assoc arg :ortho-norm-vecs onv, :valid-vecs (assoc valid-vecs j false), :curr-vec curr-vec))))

(defn orthogonal-projections [orig-vectors multiplier-cutoff tolerance]
  (let [hvc (partial handle-valid-vec orig-vectors)
        dimension (count-cols orig-vectors)
        number-vecs (count-rows orig-vectors)
        ortho-norm-vecs (matrix number-vecs dimension (repeat 0))
        valid-vecs (repeat number-vecs true)
        curr-vec (repeat dimension 0)
        _ (prn orig-vectors ortho-norm-vecs valid-vecs tolerance multiplier-cutoff curr-vec)
        init-arg (struct orthog-proj-args orig-vectors ortho-norm-vecs valid-vecs tolerance multiplier-cutoff curr-vec)]
  (loop [j 0, arg init-arg, projected-vecs []]
    (if (= j number-vecs)
      (struct orthogonal-projections projected-vecs (arg :valid-vecs))
      (let [new-arg (if (valid-vecs j)
                      (handle-valid-vec arg j)
                      arg)]
        (recur (inc j), arg, (conj projected-vecs (new-arg :curr-vec))))))))
            
