(ns quant.math.integrals.gauss-polynomials.hermite
	(:use [quant.math.distributions.gamma :only (log-value)]
				[incanter.core :only (pow, exp)]))

(declare params-valid?, throw-illegal-arg)

(defn w [s x]
	""
	(if (params-valid? s)
		(* (pow x s) (exp (- x)))
		(throw-illegal-arg)))

(defn mu-0 [s]
	""
	(if (params-valid? s)
		(exp (log-value (inc s)))
		(throw-illegal-arg)))

(defn compute [param-type mu i]
	""
	(if (params-valid? mu)
		(if (= param-type :alpha)
			0
			(if (zero? (mod i 2))
				(+ (/ i 2) mu)
				(/ i 2)))
		(throw-illegal-arg)))
		

(defn params-valid? [s]
	(> s -0.5))

(defn throw-illegal-arg []
  (throw (IllegalArgumentException. "Parameter must be bigger than -1")))


