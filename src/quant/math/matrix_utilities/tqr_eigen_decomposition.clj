; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix-utilities.tqr-eigen-decomposition
  (:use [quant.common :only (sqr half)]
        [quant.math.matrix :only (column assoc-column)]
        [incanter.core :only (sqrt abs)]
        [quant.math.matrix :only (matrix, set-main-diag)]))

(declare tqr-eigen-decomposition, make-ev, eigen-decomp-iter, comp-q, off-diag-zero?,
         qr-transform, qr-transf-iter)

(def eigen-vector-calculation #{:with-eigen-vector
                                :without-eigen-vector
                                :only-first-row-eigen-vector})
(def shift-strategy #{:no-shift :over-relaxation :close-eigen-value})

(defstruct eigen-decomp :d :ev :iter)

(defn tqr-eigen-decomp 
  ([diag sub]
    (tqr-eigen-decomp diag sub :with-eigen-vector :close-eigen-value))
  ([diag sub calc]
    (tqr-eigen-decomp diag sub calc :close-eigen-value))
  ([diag sub calc strategy]
    (tqr-eigen-decomposition diag sub calc strategy)))

(defn eigen-values [decomp]
  (decomp :d))

(defn eigen-vectors [decomp]
  (decomp :ev))
    
  
(defn update-ev [ev i sine cosine]
  "Implements the loop which updates the ev matrix
   as part of the QR transformation"
  ; ev may be a matrix or a scalar
  (if (not (coll? ev))
    ev
    (let [tmp (column ev (dec i))
          f (fn [op a b x t]
              (op (* a x) (* b t)))    
          col-i-1 (map (partial f + sine cosine) (column ev i) tmp) 
          col-i (map (partial f - cosine sine) (column ev i) tmp)
          ev1 (assoc-column ev col-i-1 (dec i))]
      (assoc-column ev1 col-i i))))



(defn sort-eigens [{:keys [ev d]}]
  (if (not (coll? ev))
    {:d d, :ev ev}
    (let [pairs (map vector d ev) 
          sorted-pairs (sort pairs)
          d1 (map first sorted-pairs)
          sign (fn [x] (if (< x 0) -1 1))
          mult-sign-of-first (fn [coll]
                               (let [s (sign (first coll))]
                                 (map #(* s %) coll)))
          sorted-ev (map second sorted-pairs)]
      { :d (vec (map first sorted-pairs))
        :ev (vec (map mult-sign-of-first sorted-ev))})))

(defn tqr-eigen-decomposition [diag sub calc strat]
  (if (not= (count diag) (inc (count sub)))
    (throw (IllegalArgumentException. "Wrong dimensions"))
    (let [n (count diag)
          ev-init (make-ev diag calc)
          e-init (vec (cons 0 sub))
          decomp (fn [k eigen e]
                   (if (zero? k)
                     eigen
                     (let [res (eigen-decomp-iter eigen e k n strat)]
                       (recur (dec k) (:eigen res) (:e res)))))
          loop-res (decomp (dec (count diag)) {:ev ev-init, :d diag, :iter 0} e-init)]
      (assoc (sort-eigens loop-res) :iter (loop-res :iter)))))

(defn- make-ev [d calc]
  "Creates the ev matrix. Args are the diagonal and the calculation method"
  (let [nb-rows-map {:with-eigen-vector (count d),
                     :without-eigen-vector 0,
                     :only-first-row-eigen-vector 1}
        nb-rows (nb-rows-map calc)]
    (if (zero? nb-rows)
      0
      (let [m (matrix nb-rows (count d) (repeat 0))]
        (set-main-diag m (repeat 1))))))

(defn- eigen-decomp-iter [eigen e k n strat]
  (if (off-diag-zero? k (eigen :d) e)
    {:eigen eigen, :e e}
    (let [comp-l (fn [k e]
                   (if (or (zero? k)
                           (off-diag-zero? k (eigen :d) e))
                     k
                     (recur (dec k) e)))
          l (comp-l (dec k) e)
          q (comp-q (eigen :d) e k l n strat)
          qr-tr (qr-transform eigen e k l q)
          not-recov-underflow (not (qr-tr :recover-underflow))
          d1 (if not-recov-underflow
               (assoc (qr-tr :d) k (- ((qr-tr :d) k) (qr-tr :u)))
               (qr-tr :d))
          e1 (if not-recov-underflow
               (assoc (qr-tr :e) k (qr-tr :q), l 0)
               (qr-tr :e))
          new-iter (inc (eigen :iter))]
      (recur {:iter new-iter, :d d1, :ev (qr-tr :ev)} e1 k n strat))))

(defn off-diag-zero? [k d e]
  (let [a (+ (abs (d k))
             (abs (d (dec k))))]
    (= a (+ a (abs (e k))))))

(defn comp-q [d e k l n strat]
  (if (not= strat :no-shift)
    (let [dk (d k)
          ddk (d (dec k))
          t1 (sqrt (+ (* 0.25 (+ (sqr dk) (sqr ddk)))
                      (* -0.5 ddk dk)
                      (sqr (e k))))
          t2 (half (+ dk ddk))
          lambda (if (< (abs (- (+ t2 t1) dk))
                        (abs (- t2 t1 dk)))
                   (+ t2 t1)
                   (- t2 t1))
          q (d l)]
      (if (= strat :close-eigen-value)
        (- q lambda)
        (if (= k (dec n))
          (- q (* 1.25 lambda))
          (- q lambda))))
    (d l)))

(defn qr-transform
  ([{:keys [ev d]} e k l q]
     (let [arg {:e e, :ev ev, :sine 1, :cosine 1, :d d, :u 0, :q q, :l l, :recov-undeflow false}]
      (qr-transform k (inc l) arg)))
  ([k i {:keys [e ev sine cosine d u q l recov-underflow] :as arg}]
     (if (or (> i k) recov-underflow)
       (dissoc arg :sine :cosine)
       (recur k (inc i) (qr-transf-iter i arg)))))

(defn qr-transf-iter [i {:keys [e ev sine cosine d u q l]}]
  "Implements an iteration of the QR transformation"
  (let [di (dec i)
        h (* cosine (e i))
        p (* sine (e i))
        e' (assoc e di (sqrt (+ (sqr p) (sqr q))))
        e'di (e' di)]
    (if (not (zero? e'di))
      (let [sine' (/ p e'di)
            cosine' (/ q e'di)
            g (- (d di) u)
            t (+  (* (- (d i) g) sine')
                  (* 2 cosine' h))
            u' (* sine' t)]
        {:recov-underflow false
         :sine sine'
         :cosine cosine'
         :d (assoc d di (+ g u'))
         :u u'
         :e e'
         :ev (update-ev ev i sine' cosine')
         :q (- (* cosine' t) h)
         :l l})
      ;don't need the other params because will exit the loop
      {:recov-underflow true
       :d (assoc d di (- u))
       :e (assoc e l 0)
       :ev ev})))

