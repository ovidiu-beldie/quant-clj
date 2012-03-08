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
		[incanter.core :only (abs)]))

(defrecord Segment [in])

(extend-type Segment
	Integrable

	(integrate [{:keys [in]} f a b]
		(let [dx (/ (- b a) (in :intervals))
					end (- b (* 0.5 dx))]
			(loop [x (+ a dx), sum (* 0.5 (+ (f a) (f b)))]
				(if (>= x end)
					(* sum dx)
					(recur (+ x dx) (+ sum (f x))))))))

(defn segment [intervals]
	(if (pos? intervals)
		(let [arg (make-integrator 1 1)]
			(Segment. (merge arg {:intervals intervals})))
		(throw (IllegalArgumentException. "intervals number must be higher than 0"))))

