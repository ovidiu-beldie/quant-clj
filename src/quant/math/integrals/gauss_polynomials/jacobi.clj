(ns quant.math.integrals.gauss-polynomials.jacobi
	(:use [quant.math.distributions.gamma :only (log-value)]
				[clojure.contrib.generic.math-functions :only (sqr)]
				;[clojure.contrib.logging :only (spy)]
				[incanter.core :only (pow, exp)]
				[clojure.test :only (deftest, deftest-, is, testing)]
				[clojure.stacktrace]))

(declare params-valid?, compute, get-fn, lim-ops-a-r, lim-ops-a-h, lim-ops-b-r, lim-ops-b-h, process-limit, do-if-params-valid)

;;; Public 

(defn w [[alpha beta] x]
	""
	(letfn [(func []
						(* (pow (- 1 x) alpha) (pow (inc x) beta)))]
		(do-if-params-valid [alpha beta] func)))

(defn mu-0 [[alpha beta]]
	""
	(letfn [(func []
						(let [fact1 (+ alpha beta 1)
									fact2-args [(inc alpha) (inc beta) (+ alpha beta 2)]
									fact2-terms (map log-value fact2-args)
									fact2 (- (reduce + fact2-terms) (last fact2-terms))]
							(* (pow 2 fact1) (exp fact2))))]
		(do-if-params-valid [alpha beta] func)))
		
(defn compute-param [param-type ops i]
	"Computes Jacobi parameter.
	 param-type identifies the function in the original algorithm, either alpha() or beta()"
	(letfn [(func []
					(compute param-type ops i :regular))]
	(do-if-params-valid ops func)))


(defn- compute [param-type ops i op-type]
	(let [compute-fn (get-fn param-type op-type)
				[numer denom] (compute-fn ops i)
				result-handler {:ok 			#(/ numer denom)
												:fail 		#(throw (ArithmeticException. "can't compute operand for jacobi integration"))
												:lhopital #(compute param-type ops i :lhopital)}]
		((result-handler (process-limit [numer denom] op-type)))))

(deftest- test-compute
	(testing "Alpha" 
		(is (= 0 (compute :alpha [0 0] 0 :regular)) "Can be solved using l'Hopital")	
		(is (thrown? ArithmeticException (compute :alpha [1 2] -3/2 :regular)))	
		(is (= 3/99 (compute :alpha [1 2] 3 :regular))))
	(testing "Beta" 
		(is (thrown? ArithmeticException (compute :alpha [0 0] 0 :regular)))	
		(is (thrown? ArithmeticException (compute :alpha [1 2] -3/2 :regular)))	
		(is (= 3/99 (compute :alpha [1 2] 3 :regular))))

(defn do-if-params-valid [[alpha beta] func]
	""
	(letfn [(params-valid? []
						(and 	(> alpha -1) (> beta -1)))]
		(if (params-valid?)
			(func)
			(throw (IllegalArgumentException. "Each parameter must be bigger than -1")))))

(deftest- test-do-if-params-valid
	(letfn [(func []
						(+ 1 1))]
		(is (= 2 (do-if-params-valid [0 0] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [-1 0] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [0 -1] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [-1 -1] func)))))

(defn- get-fn [param-type op-type]
	"Returns the compute limit ops fn correspondig to the args"
	(let [dispatch-map {[:alpha	:regular] 	lim-ops-a-r
											[:beta	:regular] 	lim-ops-b-r
											[:alpha	:lhopital] 	lim-ops-a-h
											[:beta	:lhopital] 	lim-ops-b-h }]
		(dispatch-map (vector param-type op-type))))


(defn- lim-ops-a-r [[alpha beta] i]
	"Compute limit operands for regular alpha"
	(let [numer (- (sqr beta) (sqr alpha))
				factor (+ i i alpha beta)
				denom (* factor (+ factor 2))]
		[numer denom]))

(deftest- test-lim-ops-a-r
	(is (= [0 0] (lim-ops-a-r [0 0] 0)))
	(is (= [0 8] (lim-ops-a-r [0 0] 1)))
	(is (= [3 99] (lim-ops-a-r [1 2] 3))))


(defn- lim-ops-b-r [[alpha beta] i]
	"Compute limit operands for regular beta"
	(let [numer (* 4 i (+ i alpha) (+ i beta) (+ alpha beta i))  
				factor (sqr (+ alpha beta i i))
				denom (* factor (- factor 1))]
		[numer denom]))

(deftest- test-lim-ops-b-r
	(is (= [0 0] (lim-ops-b-r [0 0] 0)))
	(is (= [4 12] (lim-ops-b-r [0 0] 1)))
	(is (= [1440 6480] (lim-ops-b-r [1 2] 3))))


(defn- lim-ops-a-h [[alpha beta] i]
	"Compute limit operands for l'Hopital alpha"
	(let [numer (* 2 beta)
				denom (* 2 (+ alpha beta i i 1))]
		[numer denom]))

(deftest- test-lim-ops-a-h
	(is (= [0 2] (lim-ops-a-h [0 0] 0)))
	(is (= [0 6] (lim-ops-a-h [0 0] 1)))
	(is (= [4 20] (lim-ops-a-h [1 2] 3))))


(defn- lim-ops-b-h [[alpha beta] i]
	"Compute limit operands for l'Hopital beta"
	(let [factor (+ i i alpha alpha beta)
				numer (* 4 i (+ i beta) factor)
				d (* 2 (+ i i alpha beta))
				denom (* d (dec d))]
		[numer denom]))

(deftest- test-lim-ops-b-h
	(is (= [0 0] (lim-ops-b-h [0 0] 0)))
	(is (= [8 12] (lim-ops-b-h [0 0] 1)))
	(is (= [600 306] (lim-ops-b-h [1 2] 3))))


(defn- process-limit [[numer denom] op-type]
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

(deftest- test-process-limit
	(testing "Non-nul values"
		(is (= :ok (process-limit [1 2] :regular))) 
		(is (= :ok (process-limit [1 2] :lhopital))))
	(testing "Non-nul numerator, nul denominator"
		(is (= :fail (process-limit [1 0] :regular))) 
		(is (= :fail (process-limit [1 0] :lhopital))))
	(testing "Nul numerator, nul denominator"
		(is (= :lhopital (process-limit [0 0] :regular))) 
		(is (= :fail (process-limit [0 0] :lhopital)))))	

(deftest all-tests
	(test-do-if-params-valid)
	(test-lim-ops-a-r)
	(test-lim-ops-b-r)
	(test-lim-ops-a-h)
	(test-lim-ops-b-h)
	(test-process-limit))



