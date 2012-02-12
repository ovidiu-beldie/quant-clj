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

