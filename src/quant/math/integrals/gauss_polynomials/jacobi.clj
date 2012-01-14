(ns quant.math.integrals.gauss-polynomials.jacobi
	(:use [quant.math.integrals.gauss-polynomials.core :only (GaussOrthoPolyProtocol)]
				[quant.math.distributions.gamma :only (log-value)]
				[clojure.contrib.generic.math-functions :only (sqr)]
				[incanter.core :only (pow, exp)]
				[clojure.stacktrace]))

(declare alpha-impl, beta-impl, handle-regular, handle-lhopital)

(defrecord GaussJacobiPolynomial [alpha beta])

(defn jacobi [alpha beta]
	(if (and (> alpha -1) (> beta -1))
		(GaussJacobiPolynomial. alpha beta)
		(throw (IllegalArgumentException. "Each parameter must be bigger than -1"))))

(defn legendre []
	(jacobi 0 0))

(defn chebyshev []
	(jacobi -0.5 -0.5))

(defn chebyshev2nd []
	(jacobi 0.5 0.5))

(defn gegenbauer [lambda]
	(jacobi (- lambda 0.5) (- lambda 0.5)))

(extend-type GaussJacobiPolynomial
	GaussOrthoPolyProtocol

	(compute-alpha [{:keys [alpha beta]} i]
		(alpha-impl alpha beta i))

	(compute-beta[{:keys [alpha beta]} i]
		(beta-impl alpha beta i))

	(mu-0 [{:keys [alpha beta]}]
		(let [fact1 (+ alpha beta 1)
					fact2-args [(inc alpha) (inc beta) (+ alpha beta 2)]
					fact2-terms (map log-value fact2-args)
					fact2 (- (reduce + fact2-terms) (* 2 (last fact2-terms)))]
			(* (pow 2 fact1) (exp fact2))))

	(w [{:keys [alpha beta]} x]
		(* (pow (- 1 x) alpha) (pow (inc x) beta))))


(defn alpha-impl 
	([alpha beta i]
	"Compute limit operands for regular alpha"
		(let [numer (- (sqr beta) (sqr alpha))
					factor (+ i i alpha beta)
					denom (* factor (+ factor 2))]
			(handle-regular numer denom alpha beta i alpha-impl)))

	([alpha beta i _]
	"Compute limit operands for l'Hopital alpha"
	  (let [numer (* 2 beta)
        denom (* 2 (+ alpha beta i i 1))]
			(handle-lhopital numer denom))))
		
(defn beta-impl
	([alpha beta i]
	"Compute limit operands for regular beta"
		(let [numer (* 4 i (+ i alpha) (+ i beta) (+ alpha beta i))  
					factor (sqr (+ alpha beta i i))
					denom (* factor (- factor 1))]
			(handle-regular numer denom alpha beta i beta-impl)))

	([alpha beta i _]
	"Compute limit operands for l'Hopital beta"	
		(let [factor (+ i i alpha alpha beta)
				numer (* 4 i (+ i beta) factor)
				d (* 2 (+ i i alpha beta))
				denom (* d (dec d))]
			(handle-lhopital numer denom))))

(defn handle-regular [numer denom alpha beta i func]
	""
	(if (zero? denom)
				(if (zero? numer)
					(func alpha beta i :lhopital)
					(throw (ArithmeticException. "can't compute operand for jacobi integration")))
				(/ numer denom)))

(defn handle-lhopital [numer denom]
	""
	(if (zero? denom)
				(throw (ArithmeticException. "can't compute operand for jacobi integration"))
				(/ numer denom)))
