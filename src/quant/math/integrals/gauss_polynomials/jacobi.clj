(ns quant.math.integrals.gauss-polynomials.jacobi
	(:use [quant.math.distributions.gamma :only (log-value)]
				[clojure.contrib.generic.math-functions :only (sqr)]
				;[clojure.contrib.logging :only (spy)]
				[incanter.core :only (pow, exp)]
				;[clojure.test :only (deftest, deftest-, is, testing)]
				[clojure.stacktrace]))

(declare params-valid?, throw-illegal-arg, compute, get-fn, lim-ops-a-r, lim-ops-a-h, lim-ops-b-r, lim-ops-b-h, process-limit)

;;; Public 

(defn w [[alpha beta] x]
	""
	(if (params-valid? [alpha beta])
		(* (pow (- 1 x) alpha) (pow (inc x) beta))
		(throw-illegal-arg)))	

(defn mu-0 [[alpha beta]]
	""
	(if (params-valid? [alpha beta])
		(let [fact1 (+ alpha beta 1)
					fact2-args [(inc alpha) (inc beta) (+ alpha beta 2)]
					fact2-terms (map log-value fact2-args)
					fact2 (- (reduce + fact2-terms) (last fact2-terms))]
			(* (pow 2 fact1) (exp fact2)))
		(throw-illegal-arg)))
		
(defn compute-param [param-type ops i]
	"Computes Jacobi parameter.
	 param-type identifies the function in the original algorithm, either alpha() or beta()"
	(if (params-valid? ops)
		(compute param-type ops i :regular)
		(throw-illegal-arg)))

;;; Private

(defn params-valid? [[alpha beta]]
	(and 	(> alpha -1) (> beta -1)))

(defn throw-illegal-arg []
	(throw (IllegalArgumentException. "Each parameter must be bigger than -1")))

(defn compute [param-type ops i op-type]
	(let [compute-fn (get-fn param-type op-type)
				[numer denom] (compute-fn ops i)
				result-handler {:ok 			#(/ numer denom)
												:fail 		#(throw (ArithmeticException. "can't compute operand for jacobi integration"))
												:lhopital #(compute param-type ops i :lhopital)}]
		((result-handler (process-limit [numer denom] op-type)))))

(defn do-if-params-valid [[alpha beta] func]
	""
	(letfn [(params-valid? []
						(and 	(> alpha -1) (> beta -1)))]
		(if (params-valid?)
			(func)
			(throw (IllegalArgumentException. "Each parameter must be bigger than -1")))))

(defn get-fn [param-type op-type]
	"Returns the compute limit ops fn correspondig to the args"
	(let [dispatch-map {[:alpha	:regular] 	lim-ops-a-r
											[:beta	:regular] 	lim-ops-b-r
											[:alpha	:lhopital] 	lim-ops-a-h
											[:beta	:lhopital] 	lim-ops-b-h }]
		(dispatch-map (vector param-type op-type))))

(defn lim-ops-a-r [[alpha beta] i]
	"Compute limit operands for regular alpha"
	(let [numer (- (sqr beta) (sqr alpha))
				factor (+ i i alpha beta)
				denom (* factor (+ factor 2))]
		[numer denom]))

(defn lim-ops-b-r [[alpha beta] i]
	"Compute limit operands for regular beta"
	(let [numer (* 4 i (+ i alpha) (+ i beta) (+ alpha beta i))  
				factor (sqr (+ alpha beta i i))
				denom (* factor (- factor 1))]
		[numer denom]))

(defn lim-ops-a-h [[alpha beta] i]
	"Compute limit operands for l'Hopital alpha"
	(let [numer (* 2 beta)
				denom (* 2 (+ alpha beta i i 1))]
		[numer denom]))

(defn lim-ops-b-h [[alpha beta] i]
	"Compute limit operands for l'Hopital beta"
	(let [factor (+ i i alpha alpha beta)
				numer (* 4 i (+ i beta) factor)
				d (* 2 (+ i i alpha beta))
				denom (* d (dec d))]
		[numer denom]))

(defn process-limit [[numer denom] op-type]
	"Process the limit's operands."
	(letfn [(denom-zero []
						(if (= op-type :lhopital)
							:fail
							(if (zero? numer)
    						:lhopital
    						:fail)))]
		(if (zero? denom)
			(denom-zero)
			:ok)))

