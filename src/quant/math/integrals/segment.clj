(ns quant.math.integrals.segment
	(:use   
		[quant.math.integrals.core]
		[incanter.core :only (abs)]
		[clojure.stacktrace]))

(defrecord Segment [in])

(defn segment [intervals]
; check intervals
	(let [arg (make-integrator 1 1


