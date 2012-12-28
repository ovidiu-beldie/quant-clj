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

(defn default-integ [f a b I N]
  (let [dx (/ (- b a) N)
        x-init (+ a (half dx))
        xs (iterate #(+ % dx) x-init)
        sum (->> (take N xs)
                 (map f)
                 (reduce + 0))]
    (half (+ I (* dx sum)))))

(defn midpoint-integ [f a b I N]
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
(def default-p {:fn default-integ, :nb-ev 2})
(def midpoint-p {:fn midpoint-integ, :nb-ev 3})
(def integ-policy {:default default-p, :midpoint midpoint-p})

(defn trapezoid [acc max-ev policy]
  "Returns a trapezoid integrator. Args are: the absolute accuracy,
the max nb of evals and a policy which can be :default or :midpoint"
  (reify Integrator
    (integrate [_ f a b]
      (let [{p-fn :fn, nb-ev :nb-ev} (integ-policy policy default-p)]
        (loop [N 1, I (init-i f a b), i 1]
          (let [newI (p-fn f a b I N)]
            (if (done? I newI acc i)
              newI
              (if (= i max-ev)
                (throw (Error. "max number of iterations reached"))
                (recur (* N nb-ev) newI (inc i))))))))))

(defn simpson [acc max-ev]
  "Returns a simpson integrator. Takes as args the absolute accuracy,
the max nb of evals"
  (reify Integrator
    (integrate [_ f a b]
      (loop [N 1, I (init-i f a b), adjI (init-i f a b), i 1]
        (let [newI ((default-p :fn) f a b I N)
              newAdjI (-> (* newI 4) (- I) (/ 3))]
          (if (done? adjI newAdjI acc i)
            newAdjI
            (if (= i max-ev)
              (throw (Error. "max number of iterations reached"))
              (recur (twice N) newI newAdjI (inc i)))))))))

