(ns quant.math.integrals.core)

(defprotocol Integrable
	(integrate [this f a b]))

(defstruct integrator :abs-accuracy :max-evals)

(defn make-integrator [abs-accuracy max-evals]
	(struct integrator abs-accuracy max-evals))
