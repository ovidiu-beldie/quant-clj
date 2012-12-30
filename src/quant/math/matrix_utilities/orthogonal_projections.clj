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
  (:use [quant.math.matrix-utilities.core :only (matrix transpose count-rows count-cols)]
        [incanter.core :only (sqrt abs)]))

(defstruct orthog-proj-args :orig-vecs :ortho-norm-vecs :valid-vecs :tolerance :multiplier-cutoff :curr-vec)

(defn inner-product [m1 r1 m2 r2]
  (reduce + (map * (m1 r1) (m2 r2))))

(defn norm-squared [m r]
  (inner-product m r m r))

(defn norm [m r]
  (sqrt (norm-squared m r)))

(defn update-ortho-norm-vecs [orig-vecs ortho-norm-vecs valid-vecs k j type]
  (let [pred (fn [l] (and (valid-vecs l) (not (= l j))))
        onv-row (if (= type :type-1) k j)
        range-max (if (= type :type-1) k (inc k))
        dot-prods (for [l (range range-max) :when (pred l)] (inner-product ortho-norm-vecs onv-row ortho-norm-vecs l))
        ordered-onv (map vector ortho-norm-vecs (range range-max))
        onv-filtered (for [onv ordered-onv :when (pred (second onv))] (first onv))
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
        onv-row (if (= type :type-1) k j)]
    (if (empty? onv-filtered)
      ortho-norm-vecs
      (let [new-row (map comp-onv-elem (ortho-norm-vecs onv-row) (transpose onv-filtered))]
      (assoc ortho-norm-vecs onv-row new-row)))))


(defn ortho-normal-basis-iter [orig-vecs tolerance j ortho-norm-vecs valid-vecs k]
  (let [onv (assoc ortho-norm-vecs k (orig-vecs k))]
    (if (and (not (= k j)) (valid-vecs k))
      (let [upd-onv (update-ortho-norm-vecs orig-vecs onv valid-vecs k j :type-1)
            norm-before-scaling (norm upd-onv k)]
        (if (< norm-before-scaling tolerance)
          {:valid-vecs (assoc valid-vecs k false) :onv upd-onv}
          (let [norm-before-scaling-recip (/ 1 norm-before-scaling)
                row-k (map #(* % norm-before-scaling-recip) (upd-onv k))]
            {:valid-vecs valid-vecs, :onv (assoc upd-onv k row-k)})))
      {:valid-vecs valid-vecs, :onv onv})))
      
(defn ortho-normal-basis-loop [{:keys [orig-vecs valid-vecs ortho-norm-vecs tolerance]} j]
  (let [num-vecs (count-rows orig-vecs)
        onbi (partial ortho-normal-basis-iter orig-vecs tolerance j)]
    (loop [k 0, vecs {:valid-vecs valid-vecs, :onv ortho-norm-vecs}]
      (if (= k num-vecs)
        vecs
        (recur (inc k) (onbi (vecs :onv) (vecs :valid-vecs) k))))))

(defn handle-valid-vec [orig-vecs {:keys [orig-vecs ortho-norm-vecs valid-vecs curr-vec multiplier-cutoff] :as arg} j]
  (let [res (ortho-normal-basis-loop arg j)
        onv (update-ortho-norm-vecs orig-vecs (res :onv) (res :valid-vecs) (dec (count-rows orig-vecs)) j :type-2)
        proj-on-orig-dir (inner-product orig-vecs j onv j)
        size-muliplier (/ (norm-squared orig-vecs j) proj-on-orig-dir)
        ]
    (if (< (abs size-muliplier) multiplier-cutoff)
      (assoc arg :ortho-norm-vecs onv, :valid-vecs valid-vecs, :curr-vec (map #(* % size-muliplier) (onv j)))
      (assoc arg :ortho-norm-vecs onv, :valid-vecs (assoc valid-vecs j false), :curr-vec curr-vec))))

(defn orthogonal-projections [orig-vectors multiplier-cutoff tolerance]
  (let [hvc (partial handle-valid-vec orig-vectors)
        dimension (count-cols orig-vectors)
        number-vecs (count-rows orig-vectors)
        ortho-norm-vecs (matrix number-vecs dimension (repeat 0))
        valid-vecs (vec (repeat number-vecs true))
        curr-vec (repeat dimension 0)
        init-arg (struct orthog-proj-args orig-vectors ortho-norm-vecs valid-vecs tolerance multiplier-cutoff curr-vec)]
  (loop [j 0, arg init-arg, projected-vecs []]
    (if (= j number-vecs)
      {:vectors projected-vecs :valid-vecs (arg :valid-vecs)}
      (let [new-arg (if (valid-vecs j)
                      (handle-valid-vec orig-vectors arg j)
                      arg)]
        (recur (inc j), new-arg, (conj projected-vecs (new-arg :curr-vec))))))))
            
