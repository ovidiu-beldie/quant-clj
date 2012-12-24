; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.trapezoid
  (:use
    [quant.common :only (half twice)]
    [quant.math.integrals.core]
    [incanter.core :only (abs)]))

(defstruct integration-policy :policy-integrate :policy-nb-evals)

(defn default-integrate [f a b I N]
  (let [dx (/ (- b a) N)
        x-init (+ a (half dx))
        xs (iterate #(+ % dx) x-init)
        sum (->> (take N xs)
                 (map f)
                 (reduce + 0))]
    (half (+ I (* dx sum)))))

(def default-policy (struct integration-policy default-integrate, 2))
  
(defn mid-point-integrate [f a b I N]
  (let [dx (/ (- b a) N)
        x-init (+ a (/ dx 6))
        D (* 2 dx 1/3)
        xs (iterate #(+ % dx) x-init)
        red (fn [s x] (+ s (+ (f x) (f (+ x D)))))
        sum (reduce red 0 (take N xs))]
    (/ (+ I (* dx sum)) 3)))

(defn done? [x y z i]
  (and (<= (abs (- x y)) z)
        (> i 5)))

(defn init-i [f a b]
  (* (+ (f a) (f b)) 
      (- b a) 
      (double 1/2)))

(defrecord Trapezoid
    [in])

(extend-type Trapezoid
  Integrable

  (integrate [{:keys [in]} f a b]
    (let [integ (partial (in :policy-integrate) f a b)]
      (loop [N 1, I (init-i f a b), i 1]
        (let [newI (integ I N)]
          (if (done? I newI (in :abs-accuracy) i)
            newI
            (if (= i (in :max-evals))
              (throw (Error. "max number of iterations reached"))
              (recur (* N (in :policy-nb-evals)) newI (inc i))))))))) 

(defn trapezoid [abs-accuracy max-evals integ-policy]
  (let [arg (make-integrator abs-accuracy max-evals)]
    (Trapezoid. (merge arg integ-policy))))

(defrecord Simpson [in])

(extend-type Simpson
  Integrable

  (integrate [{:keys [in]} f a b]
    (let [integ (partial (in :policy-integrate) f a b)]
      (loop [N 1, I (init-i f a b), adjI (init-i f a b), i 1]
        (let [newI (integ I N)
              newAdjI (-> (* newI 4) (- I) (/ 3))]
          (if (done? adjI newAdjI (in :abs-accuracy) i)
            newAdjI
            (if (= i (in :max-evals))
              (throw (Error. "max number of iterations reached"))
              (recur (twice N) newI newAdjI (inc i)))))))))

(defn simpson [abs-accuracy max-evals]
  (let [arg (make-integrator abs-accuracy max-evals)]
    (Trapezoid. (merge arg default-policy))))
