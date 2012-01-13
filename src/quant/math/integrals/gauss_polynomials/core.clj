(ns quant.math.integrals.gauss-polynomials.core
	;(:require [quant.math.integrals.gauss-polynomials.jacobi :as jacobi])
	(:use [incanter.core :only (sqrt)]
				;[clojure.contrib.logging :only (spy)]
				[clojure.stacktrace]))

(declare get-value-fn, vals-lazy-seq)

(defprotocol GaussOrthoPolyProtocol
	(compute-alpha [thisi i])
	(compute-beta [this i])
	(mu-0 [this])
	(w [this x]))

(comment
(defn value [ops n x poly-type]
	"ops is a vector containing the alpha and beta values"
	(let [fns {:jacobi jacobi/compute-param}
				get-fn (fn [param-type]
								 (partial (fns poly-type) param-type ops))
				alpha-fn (get-fn :alpha)
				beta-fn (get-fn :beta)]
		(nth (vals-lazy-seq x alpha-fn beta-fn) n)))

(defn w [ops x poly-type]
	""
	(let [fns {:jacobi jacobi/w}]
		((fns poly-type) ops x)))

(defn weighted-val [ops n x poly-type]
	""
	(* (sqrt (w ops x poly-type)) (value ops n x poly-type)))

(defn- vals-lazy-seq [x alpha-fn beta-fn]	
	"Inspired by 'Programming clojure', Stu Halloway, page 136"
	(let [item1 1
				item2	(- x (alpha-fn 0))]
		(letfn [(values
							([]
								(concat [item1 item2] (values item1 item2 2)))
						([a b n]
							(let [ newest (- x 
															(* (alpha-fn (dec n)) b) 
															(* (beta-fn (dec n)) a))]
								(lazy-seq 
									(cons newest (values b newest (inc n)))))))]
		(values))))	)
