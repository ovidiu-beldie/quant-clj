(ns quant.math.integrals.gauss-polynomials
	(:import [cern.jet.stat.tdouble Gamma])
	(:use	[quant.math.integrals.gauss-polynomials-impl]
				[incanter.core :only (pow, exp, sqrt, abs)]))

;; Protocol implemented by all integral types
(defprotocol GaussOrthogonalPolynomial
	(mu-0 [this])
	(alpha [this i])
	(beta [this i])
	(w [this x]))

;;; Integral types

;; Laguerre
(defrecord GaussLaguerrePolynomial [s])

(extend-type GaussLaguerrePolynomial
	GaussOrthogonalPolynomial

	(mu-0 [{:keys [s]}]
		(exp (Gamma/logGamma (inc s))))

	(alpha [{:keys [s]} i]
		(do-if-int i #(+ i i 1 s)))

	(beta [{:keys [s]} i]
		(do-if-int i #(* i (+ i s))))

	(w [{:keys [s]} x]
		(* (pow x s) (exp (- x)))))

;; Hermite
(defrecord GaussHermitePolynomial [mu])

(extend-type GaussHermitePolynomial
	GaussOrthogonalPolynomial

	(mu-0 [{:keys [mu]}]
		(exp (Gamma/logGamma (+ mu 0.5))))

	(alpha [_ i]
		(do-if-int i #(+ 0)))

	(beta [{:keys [mu]} i]
		(letfn [(func []
							(if (odd? i)
								(+ (/ i 2) mu)          
  	    				(/ i 2)))]
			(do-if-int i func)))

	(w [{:keys [mu]} x]
		(let [fact1 (pow (abs x) (* 2 mu))
					fact2 (exp (* (- x) x))]
			(* fact1 fact2))))

;; Jacobi
(defrecord GaussJacobiPolynomial [a b])

(extend-type GaussJacobiPolynomial
	GaussOrthogonalPolynomial

	(mu-0 [{:keys [a b]}]
		(let [fact1 (+ a b 1)
					fact2-args [(inc a) (inc b) (+ a b 2)]
					fact2-terms (map #(Gamma/logGamma %) fact2-args)
					fact2 (- 
										(reduce + fact2-terms) 
										(* 2 (last fact2-terms)))]
			(* (pow 2 fact1) (exp fact2))))

	(alpha [{:keys [a b]} i]
		(do-if-int i #(alpha-jacobi a b i)))

	(beta [{:keys [a b]} i]
		(do-if-int i #(beta-jacobi a b i)))

	(w [{:keys [a b]} x]
		(* (pow (- 1 x) a) (pow (inc x) b))))

;; Hyperbolic
(defrecord GaussHyperbolicPolynomial [])

(extend-type GaussHyperbolicPolynomial
	GaussOrthogonalPolynomial

	(mu-0 [_]
		Math/PI)

	(alpha [_ i]
		(do-if-int i #(+ 0)))
	
	(beta [_ i]
		(letfn [(func []
							(if (zero? i)
								Math/PI
								(let [half-pi (/ Math/PI 2)]
									(* half-pi half-pi i i))))]
			(do-if-int i func)))
	
	(w [_ x]
		(/ 1 (Math/cosh x))))

;;; The following functions are the equivalent of constructors for integral types

(defn laguerre [s]
	(if (> s -1)
		(GaussLaguerrePolynomial. s)
		(throw (IllegalArgumentException. "s must be superior to -1"))))

(defn hermite [mu]
	(if (> mu -0.5)
		(GaussHermitePolynomial. mu)
		(throw (IllegalArgumentException. "mu must be superior to -0.5"))))

(defn jacobi [a b]
	(if (and (> a -1) (> b -1))
		(GaussJacobiPolynomial. a b)
		(throw (IllegalArgumentException. "Both alpha and beta must be superior to -1"))))

(defn legendre []
	(jacobi 0 0))

(defn chebyshev []
	(jacobi -0.5 -0.5))

(defn chebyshev2nd []
	(jacobi 0.5 0.5))

(defn gegenbauer [lambda]
	(jacobi (- lambda 0.5) (- lambda 0.5)))

(defn hyperbolic []
	(GaussHyperbolicPolynomial. ))

;;; The following fns are built on top of the fns implementing the GaussOrthogonalPolynomial protocol. They also take a GaussOrthogonalPolynomial object as first arg

(defn value [p n x]
	(let [e1 1
				e2	(- x (alpha p 0))]
		(letfn [(values
						;Inspired by 'Programming clojure', Stu Halloway, page 136
							([]
								(concat [e1 e2] (values e1 e2 2)))
							([a b n]
								(let [ newest (- 	(* (- x (alpha p (dec n))) b)
																	(* (beta p (dec n)) a))]
									(lazy-seq 
										(cons newest (values b newest (inc n)))))))]
		(nth (values) n))))

(defn weighted-val [p n x]
	(* (sqrt (w p x)) (value p n x)))

