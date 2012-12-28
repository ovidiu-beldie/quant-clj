; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.core
  (:use
   [quant.common :only (half twice)])
  (:require
   [quant.math.integrals.kronrod :as k]
   [quant.math.integrals.trapezoid :as t]))

(defprotocol Integrator
  "Protocol implemented by all integrators"
  (integrate [this f a b]))

(defn kronrod-non-adaptive [abs-acc max-ev rel-acc]
  "Returns a Kronrod non adaptive integrator. Takes as args the absolute accuracy,
the max nb of evals and the relative accuracy"
  (reify Integrator
    (integrate [_ f a b]
      (let [hl (half (- b a))
            center (half (+ b a))]
        (loop [i 0, old-p nil]
          (let [p (k/formula (k/points i) f hl center old-p)]
            (if (k/convergent? abs-acc rel-acc p)
              (:res p)
              (recur (inc i) p))))))))

(defn kronrod-adaptive [acc max-ev]
  "Returns a Kronrod adaptive integrator. Takes as args the absolute accuracy,
the max nb of evals"
  (if (< max-ev 15)
    (throw (IllegalArgumentException. "max-evals must be >= than 15"))
    (reify Integrator
      (integrate [_ f a b]
        (k/integ f a b acc max-ev 0)))))

(defn segment [in]
  "Returns a segment integrator. Takes as arg is the nr of intervals"
  (if (pos? in)
    (reify Integrator
      (integrate [_ f a b]
        (let [dx (/ (- b a) in)
              end (- b (half dx))
              xs (iterate #(+ % dx) (+ a dx))
              sum (->> (take-while #(< % end) xs)
                       (map f)
                       (reduce + (half (+ (f a) (f b)))))]
          (double (* sum dx)))))
    (throw (IllegalArgumentException. "intervals nr must be > than 0"))))

(defn simpson [acc max-ev]
  "Returns a Simpson integrator. Takes as args the absolute accuracy,
the max nb of evals"
  (reify Integrator
    (integrate [_ f a b]
      (loop [N 1, I (t/init-i f a b), adjI (t/init-i f a b), i 1]
        (let [newI ((get-in t/integ-policy [ :default :fn]) f a b I N)
              newAdjI (-> (* newI 4) (- I) (/ 3))]
          (if (t/done? adjI newAdjI acc i)
            newAdjI
            (if (= i max-ev)
              (throw (Error. "max number of iterations reached"))
              (recur (twice N) newI newAdjI (inc i)))))))))

(defn trapezoid [acc max-ev policy]
  "Returns a trapezoid integrator. Args are: the absolute accuracy,
the max nb of evals and a policy which can be :default or :midpoint"
  (reify Integrator
    (integrate [_ f a b]
      (let [{p-fn :fn, nb-ev :nb-ev} (t/integ-policy policy (:default t/integ-policy))]
        (loop [N 1, I (t/init-i f a b), i 1]
          (let [newI (p-fn f a b I N)]
            (if (t/done? I newI acc i)
              newI
              (if (= i max-ev)
                (throw (Error. "max number of iterations reached"))
                (recur (* N nb-ev) newI (inc i))))))))))