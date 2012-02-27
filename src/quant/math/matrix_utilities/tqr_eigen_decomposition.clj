(ns quant.math.matrix-utilities.tqr-eigen-decomposition
  (:use [quant.math.matrix :only (column assoc-column)]
        [incanter.core :only (sqrt abs)]))

(def eigen-vector-calculation #{:with-eigen-vector :without-eigen-vector :only-first-row-eigen-vector})
(def shift-strategy #{:no-shift :over-relaxation :close-eigen-value})

(defn sqr [x] (* x x))

(defn tqr-eigen-decomp 
  ([diag sub]
    (tqr-eigen-decomp diag sub :with-eigen-vector :close-eigen-value))
  ([diag sub calc]
    (tqr-eigen-decomp diag sub calc :close-eigen-value))
  ([diag sub calc strategy]))
    
  
(defn update-ev [ev i sine cosine]
  (let [tmp (column ev (dec i))
        f (fn [op a b x t]
            (op (* a x) (* b t)))    
        col-i-1 (map (partial f + sine cosine) (column ev i) tmp) 
        col-i (map (partial f - cosine sine) (column ev i) tmp)
        ev1 (assoc-column ev col-i-1 (dec i))]
    (assoc-column ev1 col-i i)))

(defn qr-transf-iter [e ev sine cosine d u q i l]
  (let [dec-i (dec i)
        h (* cosine (e i))
        p (* sine (e i))
        e1 (assoc e dec-i (sqrt (+ (sqr p) (sqr q))))
        _ (prn "h=" h "p=" p "e1=" e1 "(e i)=" (e i))]
    (if (not (zero? (e1 dec-i)))
      (let [sine (/ p (e1 dec-i))
            cosine (/ q (e1 dec-i))
            g (- (d dec-i) u)
            t (+  (* (- (d i) g) sine) 
                  (* 2 cosine h))
            u1 (* sine t)
            _ (prn "u1=" u1)
            d1 (assoc d dec-i (+ g u1))
            q1 (- (* cosine t) h)
            ev1 (update-ev ev i sine cosine)]
        {:recov-underflow false, :sine sine, :cosine cosine, :d d1, :u u1, :e e1, :ev ev1 :q q1})
      (let [d1 (assoc d (dec i) (- u))
            e1 (assoc e l 0)]
        ;don't need the other params because will exit the loop
        {:recov-underflow true, :d d1, :e e1}))))


(defn qr-transform [d ev e k l q]
  (loop [i (inc l), ev1 ev, e1 e, d1 d, sine 1, cosine 1, u 0, q1 q, recov-underflow false]
    (let [h (* cosine (e i))
          p (* sine (e i))
          e2 (assoc e1 (dec i) (sqrt (+ (sqr p) (sqr q1))))
          res (qr-transf-iter e2 ev1 h p d1 u q1 i)]
      (if (or (> i k) (res :recover-underflow))
        (dissoc res :sine :cosine :u)
        (recur (inc i), (res :ev), (res :e), (res :d), (res :sine), 
              (res :cosine), (res :u), (res :q), (res :recov-underflow))))))
        
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

(defn eigen-decomp-iter [k n iter-p e-p ev-p d-p strat]
  (loop [iter iter-p, d d-p, e e-p, ev ev-p]
    (if (off-diag-zero? k d e)
      {:iter iter, :d d, :e e, :ev ev}
      (let [comp-l (fn [k e]
                     (loop [l (dec k)]
                       (if (or (zero? l) (off-diag-zero? l e))
                         l
                         (recur (dec l)))))
            l (comp-l k e)
            q (comp-q d e k l n strat)
            qr-tr (qr-transform d ev e k l q)
            not-recov-underflow (not (qr-tr :recover-underflow))
            d1 (if not-recov-underflow
                 (assoc (qr-tr :d) k (- ((qr-tr :d) k) (qr-tr :u)))
                 (qr-tr :d))
            e1 (if not-recov-underflow
                 (assoc (qr-tr :e) k q, l 0)
                 (qr-tr :e))]
         (recur (inc iter) d1 e1 (qr-tr :ev))))))    
        
;(defn eigen-decomp-loop [n e ev d]
;  (loop [k (dec n)]
;    (if (< k 1)
;      {:d d, :ev ev}))))


