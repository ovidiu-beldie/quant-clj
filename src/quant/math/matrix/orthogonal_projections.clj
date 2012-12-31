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

(defn norm-squared [m r]
  (inner-prod (m r) (m r)))

(defn norm [m r]
  (sqrt (norm-squared m r)))

(defn orthogonal-projections [orig-vecs multiplier-cutoff tolerance]
  "Performs the orthogonal projections algo on a collection of vectors.
Args are: original vecs, multiplier cutoff and tolerance"
  (let [dim (count-cols orig-vecs)
        nr-vecs (count-rows orig-vecs)
        vv (vec (repeat nr-vecs true))
        init {:ov orig-vecs :mc multiplier-cutoff :tol tolerance :vv vv
              :onv (matrix nr-vecs dim (repeat 0))
              :cv (repeat dim 0)}]
  (loop [j 0, args init, proj-v []]
    (if (= j nr-vecs)
      {:vectors proj-v :vv (args :vv)}
      (let [args' (if (vv j) (handle-valid-v args j) args)]
        (recur (inc j), args', (conj proj-v (args' :cv))))))))

(defn- handle-valid-v [{:keys [ov onv vv cv mc tol] :as arg} j]
  "Handle valid vecs.
Args are: original vecs, ortho-normalized vec, valid vecs,
current vec, multiplier cutoff, tolerance and j (an index)"
  (let [num-vecs (count-rows ov)
        onbi (partial onb-iter ov tol j)
        onb-loop (fn [k vecs]
                     (if (= k num-vecs)
                       vecs
                       (recur (inc k) (onbi k vecs))))
        r (onb-loop 0 {:onv onv, :vv vv})
        onv' (update-onv (:onv r) (:vv r) (dec (count-rows ov)) j :t2)
        proj (inner-prod (ov j) (onv' j)) ;proj-on-orig-direction
        size-m (/ (norm-squared ov j) proj) ;size-multiplier
        [vv' cv'] (if (< (abs size-m) mc)
                    [vv, (map #(* % size-m) (onv' j))]
                    [(assoc vv j false), cv])]
    (assoc arg :onv onv' :vv vv' :cv cv')))

(defn- onb-iter [ov tol j k {:keys [onv vv]}]
  "Ortho-normal basis iteration.
Args are: original vec, tolerance, j, ortho-normalized vecs, valid vecs, k"
  (let [onv' (assoc onv k (ov k))]
    (if (and (not= k j) (vv k))
      (let [upd-onv (update-onv onv' vv k j :t1)
            nbs (norm upd-onv k)] ;norm-before-scaling
        (if (< nbs tol)
          {:vv (assoc vv k false) :onv upd-onv}
          (let [nbsr (/ 1 nbs) ;norm-before-scaling-recip
                row-k (map #(* % nbsr) (upd-onv k))]
            {:vv vv, :onv (assoc upd-onv k row-k)})))
      {:vv vv, :onv onv'})))

(defn- update-onv [onv vv k j type]
  "Update ortho-normal vecs.
Args are: original vec, ortho-normalized vec, valid vec, k, j and type"
  (let [pred (fn [l] (and (vv l) (not= l j)))
        onv-row (if (= type :t1) k j)
        r-max (range (if (= type :t1) k (inc k)))
        dot-prods (for [l r-max :when (pred l)] (inner-prod (onv onv-row) (onv l)))
        ord-onv (map vector onv r-max)
        onv-filtered (for [o ord-onv :when (pred (second o))] (first o))
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
        onv-row (if (= type :t1) k j)]
    (if (empty? onv-filtered)
      onv
      (let [row' (map comp-onv-elem (onv onv-row) (transpose onv-filtered))]
        (assoc onv onv-row row')))))