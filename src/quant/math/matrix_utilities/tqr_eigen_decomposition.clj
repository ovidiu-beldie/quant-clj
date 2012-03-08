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
  (:use [quant.math.matrix :only (column assoc-column)]
        [incanter.core :only (sqrt abs)]
        [quant.math.matrix :only (matrix, set-main-diag)]))

(declare tqr-eigen-decomposition)

(def eigen-vector-calculation #{:with-eigen-vector :without-eigen-vector :only-first-row-eigen-vector})
(def shift-strategy #{:no-shift :over-relaxation :close-eigen-value})

(defstruct eigen-decomp :d :ev :iter)

(defn sqr [x] (* x x))

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
   as partof the QR transformation"
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

(defn qr-transf-iter [i {:keys [e ev sine cosine d u q l]}]
  "Implements an iteration of the QR transformation"
  (let [dec-i (dec i)
        h (* cosine (e i))
        p (* sine (e i))
        e1 (assoc e dec-i (sqrt (+ (sqr p) (sqr q))))]
    (if (not (zero? (e1 dec-i)))
      (let [sine (/ p (e1 dec-i))
            cosine (/ q (e1 dec-i))
            g (- (d dec-i) u)
            t (+  (* (- (d i) g) sine) 
                  (* 2 cosine h))
            u1 (* sine t)
            d1 (assoc d dec-i (+ g u1))
            q1 (- (* cosine t) h)
            ev1 (update-ev ev i sine cosine)]
        {:recov-underflow false, :sine sine, :cosine cosine, :d d1, :u u1, :e e1, :ev ev1 :q q1, :l l})
      (let [d1 (assoc d (dec i) (- u))
            e1 (assoc e l 0)]
        ;don't need the other params because will exit the loop
        {:recov-underflow true, :d d1, :e e1, :ev ev}))))


(defn qr-transform [eigen, e-init, k l q-init]
  (loop [i (inc l), arg (assoc eigen :e e-init, :sine 1, :cosine 1, :u 0, :q q-init, :recov-underflow false, :l l)]
    (if (or (> i k) (arg :recover-underflow))
      (dissoc arg :sine :cosine)
      (let [res (qr-transf-iter i arg)]
        (recur (inc i), res)))))
        
(defn comp-q [d e k l n strat]
  (let [q (d l)]
    (if (not (= strat :no-shift))
      (let [t1 (sqrt (+ (* 0.25 (+ (sqr (d k)) (sqr (d (dec k)))))
                        (* (- 0.5) (d (dec k)) (d k))
                        (sqr (e k))))
            t2 (* 0.5 (+ (d k) (d (dec k))))
            lambda (if (< (abs (- (+ t2 t1) (d k))) (abs (- t2 t1 (d k))))
                     (+ t2 t1)
                     (- t2 t1))]
        (if (= strat :close-eigen-value)
          (- q lambda)
          (if (= k (dec n))
            (- q (* 1.25 lambda))
            (- q (* 1 lambda)))))
      q)))

(defn off-diag-zero? [k d e]
  (=  (+ (abs (d (dec k))) (abs (d k)))
      (+ (abs (d (dec k))) (abs (d k)) (abs (e k)))))

(defn eigen-decomp-iter [eigen-init e-init k n strat]
  (do
  (loop [eigen eigen-init, e e-init]
    (if (off-diag-zero? k (eigen :d) e)
      {:eigen eigen, :e e}
      (let [comp-l (fn [k e]
                     (loop [l (dec k)]
                       (if (or (zero? l) (off-diag-zero? l (eigen :d) e))
                         l
                         (recur (dec l)))))
            l (comp-l k e)
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
         (recur {:iter new-iter, :d d1, :ev (qr-tr :ev)} e1))))))    
      
  
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

(defn make-ev [d calc]
  (let [nb-rows-map {:with-eigen-vector (count d), :without-eigen-vector 0, :only-first-row-eigen-vector 1}
        nb-rows (nb-rows-map calc)]
    (if (zero? nb-rows)
      0
      (let [m (matrix nb-rows (count d) (repeat 0))]
        (set-main-diag m (repeat 1))))))

(defn tqr-eigen-decomposition [diag sub calc strat]
  (if (not (= (count diag) (inc (count sub))))
    (throw (IllegalArgumentException. "Wrong dimensions"))
    (let [n (count diag)
          ev-init (make-ev diag calc)
          e-init (vec (cons 0 sub))
          loop-res  (loop [k (dec (count diag)), eigen {:ev ev-init, :d diag, :iter 0}, e e-init]
                      (if (= k 0)
                        eigen
                        (let [res (eigen-decomp-iter eigen e k n strat)]
                          (recur (dec k) (res :eigen) (res :e)))))]
      (assoc (sort-eigens loop-res) :iter (loop-res :iter)))))
  

