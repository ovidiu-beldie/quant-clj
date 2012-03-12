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
;        [clojure.contrib.logging :only (spy)]
        [incanter.core :only (sqrt abs)]))

(defstruct orthog-proj-args :orig-vecs :ortho-norm-vecs :valid-vecs :tolerance :multiplier-cutoff :curr-vec)
(defstruct orthogonal-projections :projected-vecs :valid-vecs)


(defn inner-product [m1 r1 m2 r2]
  (do
  ;(prn "m1=" m1 "r1=" r1 "m2=" m2 "r2=" r2)
  ;(prn "inner-product=" (reduce + (map * (m1 r1) (m2 r2))))
  (reduce + (map * (m1 r1) (m2 r2)))))

(defn norm-squared [m r]
  (inner-product m r m r))

(defn norm [m r]
  (sqrt (norm-squared m r)))


(defn update-ortho-norm-vecs [orig-vecs ortho-norm-vecs valid-vecs k j type]
  (let [pred (fn [l] (and (valid-vecs l) (not (= l j))))
        _ (prn "update : entered: onv=" ortho-norm-vecs "k=" k "j=" j)
        onv-row (if (= type :type-1) k j)
        range-max (if (= type :type-1) k (inc k))
        dot-prods (for [l (range range-max) :when (pred l)] (inner-product ortho-norm-vecs onv-row ortho-norm-vecs l))
        _ (prn "update : dot-prods=" dot-prods)
        ordered-onv (map vector ortho-norm-vecs (range range-max))
        ;_ (prn "update : ordered-onv=" ordered-onv)
        onv-filtered (for [onv ordered-onv :when (pred (second onv))] (first onv))
        ;_ (prn "update : onv-filtered=" onv-filtered)
        comp-onv-elem (fn [init-onv-elem onv-filtered-col]
                        (do
                        ;(prn "update comp-elem: init-onv-elem =" init-onv-elem "onv-filtered-col=" onv-filtered-col "dot-prods=" dot-prods)
                        ;(prn "comp-onv-elem ret:" (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
                        ;(prn "update comp-elem =" (reduce - init-onv-elem (map * dot-prods onv-filtered-col)))
                        (reduce - init-onv-elem (map * dot-prods onv-filtered-col))))
        onv-row (if (= type :type-1) k j)
        ]
    (if (empty? onv-filtered)
      (do
      (prn "update : empty=true, exit=" ortho-norm-vecs)
      ortho-norm-vecs)
      (let [_ (prn "update : calling map comp-onv-elem row=" (ortho-norm-vecs onv-row) "onv-filtered-transp=" (transpose onv-filtered))
            new-row (map comp-onv-elem (ortho-norm-vecs onv-row) (transpose onv-filtered))
            _ (prn "update : new row=" new-row)
           ]
      (assoc ortho-norm-vecs onv-row new-row)))))


(defn ortho-normal-basis-iter [orig-vecs tolerance j ortho-norm-vecs valid-vecs k]
  (let [_ (prn "Iteration start--------------------- k=" k "j=" j)
        _ (prn "iter: iter start onv=" ortho-norm-vecs)
        onv (assoc ortho-norm-vecs k (orig-vecs k))
        _ (prn "iter: before if k!=j onv=" onv)
       ]
    (if (and (not (= k j)) (valid-vecs k))
      (let [;_ (prn "XXXXXXXXX ortho-normal-basis-iter: k=" k "j="j)
            upd-onv (update-ortho-norm-vecs orig-vecs onv valid-vecs k j :type-1)
            _ (prn "iter: after l loop : onv=" upd-onv)
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
        (do
        (prn "loop end of iter-1 results=" vecs)
        (recur (inc k) (onbi (vecs :onv) (vecs :valid-vecs) k)))))))

(defn handle-valid-vec [orig-vecs {:keys [orig-vecs ortho-norm-vecs valid-vecs curr-vec multiplier-cutoff] :as arg} j]
  (let [_ (prn "hvv : enter - onv=" ortho-norm-vecs)
        res (ortho-normal-basis-loop arg j)
        _ (prn "hvv loop results=" res)
        onv (update-ortho-norm-vecs orig-vecs (res :onv) (res :valid-vecs) (dec (count-rows orig-vecs)) j :type-2)
        _ (prn "hvv after norm onv=" onv)
        proj-on-orig-dir (inner-product orig-vecs j onv j)
        size-muliplier (/ (norm-squared orig-vecs j) proj-on-orig-dir)
        _ (prn "hvv: norm-squared=" (norm-squared orig-vecs j) "proj-on-orig-dir=" proj-on-orig-dir "size-muliplier=" size-muliplier)
        ]
    (if (< (abs size-muliplier) multiplier-cutoff)
      (do
      (prn "hvv: size mult < mul-cutoff, set curr-vec,size-muliplier=" size-muliplier "multiplier-cutoff=" multiplier-cutoff "(onv j)=" (onv j))
      (assoc arg :ortho-norm-vecs onv, :valid-vecs valid-vecs, :curr-vec (map #(* % size-muliplier) (onv j))))
      (do
      (prn "hvv: size mult >= mul-cutoff, NOT set curr-vec, size-muliplier=" size-muliplier "multiplier-cutoff=" multiplier-cutoff "(onv j)=" (onv j))
      (assoc arg :ortho-norm-vecs onv, :valid-vecs (assoc valid-vecs j false), :curr-vec curr-vec)))))

(defn orthogonal-projections [orig-vectors multiplier-cutoff tolerance]
  (let [hvc (partial handle-valid-vec orig-vectors)
        dimension (count-cols orig-vectors)
        number-vecs (count-rows orig-vectors)
        ortho-norm-vecs (matrix number-vecs dimension (repeat 0))
        ;_ (prn "orthogonal-projections ortho-norm-vecs=" ortho-norm-vecs) 
        valid-vecs (vec (repeat number-vecs true))
        curr-vec (repeat dimension 0)
        ;_ (prn orig-vectors ortho-norm-vecs valid-vecs tolerance multiplier-cutoff curr-vec)
        init-arg (struct orthog-proj-args orig-vectors ortho-norm-vecs valid-vecs tolerance multiplier-cutoff curr-vec)]
  (loop [j 0, arg init-arg, projected-vecs []]
    (if (= j number-vecs)
      ;(struct orthogonal-projections projected-vecs (arg :valid-vecs))
      {:vectors projected-vecs :valid-vecs (arg :valid-vecs)}
      (let [new-arg (if (valid-vecs j)
                      (do
                      ;(prn "calling handle-valid-vec")
                      (handle-valid-vec orig-vectors arg j))
                      arg)
            _ (prn "op: end of iter j=" j "res=" new-arg)
            _ (prn "MMMMMM curr-vec=" (new-arg :curr-vec))
           ]
        (recur (inc j), new-arg, (conj projected-vecs (new-arg :curr-vec))))))))
            
