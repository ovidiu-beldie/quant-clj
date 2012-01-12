(ns quant.math.integrals.gauss-polynomials.laguerre
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

(defn compute [param-type s i]
	""
	(if (params-valid? s)
		(if (= param-type :alpha)
			(+ i i 1 s)
			(* i (+ i s)))
		(throw-illegal-arg)))
		

(defn params-valid? [s]
	(> s -1))

(defn throw-illegal-arg []
  (throw (IllegalArgumentException. "Parameter must be bigger than -1")))


