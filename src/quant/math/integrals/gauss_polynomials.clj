(ns quant.math.integrals.gauss-polynomials
	(:import [cern.jet.stat.tdouble Gamma])
	(:use	[clojure.contrib.math :only (ceil)]
				[clojure.contrib.generic.math-functions :only (sqr)]
				[incanter.core :only (pow, exp, sqrt, abs)]
				[clojure.stacktrace]))

(declare alpha-impl, alpha-jacobi, beta-impl, beta-jacobi, handle-regular, handle-lhopital, check-if-int)

;; Protocol implemented by all integral types
(defprotocol GaussOrthogonalPolynomial
	(mu-0 [this])
	(alpha-impl [this i])
	(beta-impl [this i])
	(w [this x]))

;;; Integral types

;; Laguerre
(defrecord GaussLaguerrePolynomial [s])

(extend-type GaussLaguerrePolynomial
	GaussOrthogonalPolynomial

	(mu-0 [{:keys [s]}]
		(exp (Gamma/logGamma (inc s))))

	(alpha-impl [{:keys [s]} i]
		(+ i i 1 s))

	(beta-impl [{:keys [s]} i]
		(* i (+ i s)))

	(w [{:keys [s]} x]
		(* (pow x s) (exp (- x)))))

;; Hermite
(defrecord GaussHermitePolynomial [mu])

(extend-type GaussHermitePolynomial
	GaussOrthogonalPolynomial

	(mu-0 [{:keys [mu]}]
		(exp (Gamma/logGamma (+ mu 0.5))))

	(alpha-impl [_ i]
			0)

	(beta-impl [{:keys [mu]} i]
			(if (odd? (check-if-int i))
				(+ (/ i 2) mu)          
  	    (/ i 2)))

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

	(alpha-impl [{:keys [a b]} i]
		(alpha-jacobi a b i))

	(beta-impl [{:keys [a b]} i]
			(beta-jacobi a b i))

	(w [{:keys [a b]} x]
		(* (pow (- 1 x) a) (pow (inc x) b))))

; Jacobi helper fns

(defn alpha-jacobi 
	([a b i]
	"Compute limit operands for regular alpha"
		(let [numer (- (sqr b) (sqr a))
					factor (+ i i a b)
					denom (* factor (+ factor 2))]
			(handle-regular numer denom a b i alpha-jacobi)))

	([a b i _]
	"Compute limit operands for l'Hopital alpha"
	  (let [numer (* 2 b)
        denom (* 2 (+ a b i i 1))]
			(handle-lhopital numer denom))))
		
(defn beta-jacobi
	([a b i]
	"Compute limit operands for regular beta"
		(let [numer (* 4 i (+ i a) (+ i b) (+ a b i))  
					factor (sqr (+ a b i i))
					denom (* factor (- factor 1))]
			(handle-regular numer denom a b i beta-jacobi)))

	([a b i _]
	"Compute limit operands for l'Hopital beta"	
		(let [factor (+ i i a a b)
				numer (* 4 i (+ i b) factor)
				d (* 2 (+ i i a b))
				denom (* d (dec d))]
			(handle-lhopital numer denom))))

(defn handle-regular [numer denom a b i func]
	""
	(if (zero? denom)
				(if (zero? numer)
					(func a b i :lhopital)
					(throw (ArithmeticException. "can't compute operand for jacobi integration")))
				(/ numer denom)))

(defn handle-lhopital [numer denom]
	""
	(if (zero? denom)
				(throw (ArithmeticException. "can't compute operand for jacobi integration"))
				(/ numer denom)))

;; Hyperbolic
(defrecord GaussHyperbolicPolynomial [])

(extend-type GaussHyperbolicPolynomial
	GaussOrthogonalPolynomial

	(mu-0 [_]
		Math/PI)

	(alpha-impl [_ i]
		0)
	
	(beta [_ i]
		(if (zero? i)
				Math/PI
				(let [half-pi (/ Math/PI 2)]
					(* half-pi half-pi i i))))
	
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

(defn alpha [p i]
	(do
		(check-if-int i)
		(alpha-impl p i)))

(defn beta [p i]
	(do
		(check-if-int i)
		(beta-impl p i)))

(defn value [i n x]
	(let [e1 1
				e2	(- x (alpha i 0))]
		(letfn [(values
						;Inspired by 'Programming clojure', Stu Halloway, page 136
							([]
								(concat [e1 e2] (values e1 e2 2)))
							([a b n]
								(let [ newest (- x 
																(* (alpha i (dec n)) b) 
																(* (beta i (dec n)) a))]
									(lazy-seq 
										(cons newest (values b newest (inc n)))))))]
		(nth (values) n))))

(defn weighted-val [i n x]
	(* (sqrt (w i x)) (value i n x)))

; Helper fns
(defn check-if-int [n]
	(if (= n (ceil n))
		n
		(throw (IllegalArgumentException. "Parameter must be integer"))))

