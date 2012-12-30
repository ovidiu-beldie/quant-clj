; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix.orthogonal-projections
  (:use [quant.math.matrix.core :only (matrix transpose count-rows count-cols)]
        [incanter.core :only (sqrt abs)]))

(declare handle-valid-v update-ortho-norm-v ortho-normal-basis-loop ortho-normal-basis-iter)

(defstruct orthog-proj-args :orig-v :ortho-norm-v :valid-v :tol :m-cutoff :curr-v)

(defn inner-product [m1 r1 m2 r2]
  (reduce + (map * (m1 r1) (m2 r2))))

(defn norm-squared [m r]
  (inner-product m r m r))

(defn norm [m r]
  (sqrt (norm-squared m r)))

(defn orthogonal-projections [orig-v m-cutoff tol]
  "Performs the orthogonal projections algo on a collection of vectors.
Args are: original vecs, multiplier cutoff and tolerance"
  (let [hvc (partial handle-valid-v orig-v)
        dim (count-cols orig-v)
        nr-vecs (count-rows orig-v)
        ortho-norm-v (matrix nr-vecs dim (repeat 0))
        valid-v (vec (repeat nr-vecs true))
        curr-v (repeat dim 0)
        init-arg (struct orthog-proj-args orig-v ortho-norm-v valid-v tol m-cutoff curr-v)]
  (loop [j 0, arg init-arg, projected-vecs []]
    (if (= j nr-vecs)
      {:vectors projected-vecs :valid-v (arg :valid-v)}
      (let [new-arg (if (valid-v j)
                      (handle-valid-v arg j)
                      arg)]
        (recur (inc j), new-arg, (conj projected-vecs (new-arg :curr-v))))))))

(defn- handle-valid-v [{:keys [orig-v ortho-norm-v valid-v curr-v m-cutoff] :as arg} j]
  (let [res (ortho-normal-basis-loop arg j)
        onv (update-ortho-norm-v orig-v (res :onv) (res :valid-v) (dec (count-rows orig-v)) j :type-2)
        proj-on-orig-dir (inner-product orig-v j onv j)
        size-muliplier (/ (norm-squared orig-v j) proj-on-orig-dir)
        ]
    (if (< (abs size-muliplier) m-cutoff)
      (assoc arg :ortho-norm-v onv, :valid-v valid-v, :curr-v (map #(* % size-muliplier) (onv j)))
      (assoc arg :ortho-norm-v onv, :valid-v (assoc valid-v j false), :curr-v curr-v))))

(defn- ortho-normal-basis-loop [{:keys [orig-v valid-v ortho-norm-v tol]} j]
  (let [num-vecs (count-rows orig-v)
        onbi (partial ortho-normal-basis-iter orig-v tol j)]
    (loop [k 0, vecs {:valid-v valid-v, :onv ortho-norm-v}]
      (if (= k num-vecs)
        vecs
        (recur (inc k) (onbi (vecs :onv) (vecs :valid-v) k))))))

(defn- ortho-normal-basis-iter [orig-v tol j ortho-norm-v valid-v k]
  (let [onv (assoc ortho-norm-v k (orig-v k))]
    (if (and (not (= k j)) (valid-v k))
      (let [upd-onv (update-ortho-norm-v orig-v onv valid-v k j :type-1)
            norm-before-scaling (norm upd-onv k)]
        (if (< norm-before-scaling tol)
          {:valid-v (assoc valid-v k false) :onv upd-onv}
          (let [norm-before-scaling-recip (/ 1 norm-before-scaling)
                row-k (map #(* % norm-before-scaling-recip) (upd-onv k))]
            {:valid-v valid-v, :onv (assoc upd-onv k row-k)})))
      {:valid-v valid-v, :onv onv})))

(defn- update-ortho-norm-v [orig-v ortho-norm-v valid-v k j type]
  (let [pred (fn [l] (and (valid-v l) (not= l j)))
        onv-row (if (= type :type-1) k j)
        r-max (range (if (= type :type-1) k (inc k)))
        dot-prods (for [l r-max :when (pred l)]
                    (inner-product ortho-norm-v onv-row ortho-norm-v l))
        ordered-onv (map vector ortho-norm-v r-max)
        onv-filtered (for [onv ordered-onv :when (pred (second onv))] (first onv))
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
        onv-row (if (= type :type-1) k j)]
    (if (empty? onv-filtered)
      ortho-norm-v
      (let [new-row (map comp-onv-elem (ortho-norm-v onv-row) (transpose onv-filtered))]
      (assoc ortho-norm-v onv-row new-row)))))