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
  (:use [quant.math.matrix.core :only (matrix transpose count-rows count-cols inner-prod)]
        [incanter.core :only (sqrt abs)]))

(declare handle-valid-v update-onv onb-loop onb-iter)

(defstruct orthog-proj-args :ov :onv :vv :tol :m-cutoff :curr-v)

(defn norm-squared [m r]
  (inner-prod (m r) (m r)))

(defn norm [m r]
  (sqrt (norm-squared m r)))

(defn orthogonal-projections [ov m-cutoff tol]
  "Performs the orthogonal projections algo on a collection of vectors.
Args are: original vecs, multiplier cutoff and tolerance"
  (let [hvc (partial handle-valid-v ov)
        dim (count-cols ov)
        nr-vecs (count-rows ov)
        onv (matrix nr-vecs dim (repeat 0))
        vv (vec (repeat nr-vecs true))
        curr-v (repeat dim 0)
        init-arg (struct orthog-proj-args ov onv vv tol m-cutoff curr-v)]
  (loop [j 0, arg init-arg, projected-vecs []]
    (if (= j nr-vecs)
      {:vectors projected-vecs :vv (arg :vv)}
      (let [new-arg (if (vv j)
                      (handle-valid-v arg j)
                      arg)]
        (recur (inc j), new-arg, (conj projected-vecs (new-arg :curr-v))))))))

(defn- handle-valid-v [{:keys [ov onv vv curr-v m-cutoff tol] :as arg} j]
  (let [num-vecs (count-rows ov)
        onbi (partial onb-iter ov tol j)
        onb-loop (fn [k vecs]
                     (if (= k num-vecs)
                       vecs
                       (recur (inc k) (onbi k vecs))))
        r (onb-loop 0 {:onv onv, :vv vv})
        onv' (update-onv (:onv r) (:vv r) (dec (count-rows ov)) j :type-2)
        proj-on-orig-dir (inner-prod (ov j) (onv' j))
        size-muliplier (/ (norm-squared ov j) proj-on-orig-dir)]
    (if (< (abs size-muliplier) m-cutoff)
      (assoc arg :onv onv', :vv vv, :curr-v (map #(* % size-muliplier) (onv' j)))
      (assoc arg :onv onv', :vv (assoc vv j false), :curr-v curr-v))))

(defn- onb-iter [ov tol j k {:keys [onv vv]}]
  "Args are: original vec, tolerance, j, ortho-normalized vec, valid vec, k"
  (let [onv' (assoc onv k (ov k))]
    (if (and (not= k j) (vv k))
      (let [upd-onv (update-onv onv' vv k j :type-1)
            nbs (norm upd-onv k)] ;norm-before-scaling
        (if (< nbs tol)
          {:vv (assoc vv k false) :onv upd-onv}
          (let [nbsr (/ 1 nbs) ;norm-before-scaling-recip
                row-k (map #(* % nbsr) (upd-onv k))]
            {:vv vv, :onv (assoc upd-onv k row-k)})))
      {:vv vv, :onv onv'})))

(defn- update-onv [onv vv k j type]
  "Args are: original vec, ortho-normalized vec, valid vec, k, j and type"
  (let [pred (fn [l] (and (vv l) (not= l j)))
        onv-row (if (= type :type-1) k j)
        r-max (range (if (= type :type-1) k (inc k)))
        dot-prods (for [l r-max :when (pred l)] (inner-prod (onv onv-row) (onv l)))
        ord-onv (map vector onv r-max)
        onv-filtered (for [o ord-onv :when (pred (second o))] (first o))
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
        onv-row (if (= type :type-1) k j)]
    (if (empty? onv-filtered)
      onv
      (let [new-row (map comp-onv-elem (onv onv-row) (transpose onv-filtered))]
        (assoc onv onv-row new-row)))))