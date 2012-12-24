; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.segment
  (:use
   [quant.math.integrals.core]
   [quant.common :only (half)]))

(defn segment [in]
  ""
  (if (pos? in)
    (reify Integrable
      (integrate [_ f a b]
        (let [dx (/ (- b a) in)
              end (- b (half dx))
              xs (iterate #(+ % dx) (+ a dx))
              sum (->> (take-while #(< % end) xs)
                       (map f)
                       (reduce + (half (+ (f a) (f b)))))]
          (double (* sum dx)))))
    (throw (IllegalArgumentException. "intervals nr must be > than 0"))))

