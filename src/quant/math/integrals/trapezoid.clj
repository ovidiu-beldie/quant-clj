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
    [incanter.core :only (abs)]))

(defn- default-integ [f a b I N]
  (let [dx (/ (- b a) N)
        x-init (+ a (half dx))
        xs (iterate #(+ % dx) x-init)
        sum (->> (take N xs)
                 (map f)
                 (reduce + 0))]
    (half (+ I (* dx sum)))))

(defn- midpoint-integ [f a b I N]
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

;; Integration policies
(def ^:private default-p {:fn default-integ, :nb-ev 2})
(def ^:private midpoint-p {:fn midpoint-integ, :nb-ev 3})
(def integ-policy {:default default-p, :midpoint midpoint-p})

