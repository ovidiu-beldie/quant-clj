(ns quant.math.distributions.gamma
	(:use [incanter.core :only (log)]))

(declare compute-ser)

(defn log-value [x]
	""
	(if (<= x 0)
		(throw (IllegalArgumentException. "positive argument required"))
		(let [compute-temp (fn []
						 (let [temp (+ x 5.5)
        					 t2 (* (+ x 0.5) (log temp))]
    					(- temp t2)))
					ser-coef 2.5066282746310005
					ser (compute-ser x)
					ser-term (log (* ser  ser-coef (/ 1 x)))]
			(- ser-term (compute-temp)))))
			  	
(defn compute-ser [x]
	""
  (let  [coefs [76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5]
				 ser 1.000000000190015
				 t 0	
         ser-terms (for [c coefs :let [t (inc t)]] (/ c (+ x t)))]
    (reduce + (conj ser-terms ser))))	
