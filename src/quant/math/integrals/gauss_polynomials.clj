; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.gauss-polynomials
  (:import [cern.jet.stat.tdouble Gamma])
  (:use [quant.common :only (twice half)]
   [quant.math.integrals.gauss-polynomials-impl :only (do-if-int)]
   [incanter.core :only (pow, exp, sqrt, abs)]))

;; Protocol implemented by all integral types
(defprotocol GaussOrthoPoly
  (mu-0 [this])
  (alpha [this i])
  (beta [this i])
  (w [this x]))

;;; The following functions are the equivalent of constructors for integral types

(defn laguerre [s]
  (if (> s -1)
    (reify GaussOrthoPoly
      (mu-0 [_]
        (exp (Gamma/logGamma (inc s))))
      (alpha [_ i]
        (do-if-int i #(+ (twice i) 1 s)))
      (beta [_ i]
        (do-if-int i #(* i (+ i s))))
      (w [_ x]
        (* (pow x s) (exp (- x)))))
    (throw (IllegalArgumentException. "s must be > than -1"))))

(defn hermite [mu]
  (if (> mu -0.5)
    (reify GaussOrthoPoly
      (mu-0 [_]
        (exp (Gamma/logGamma (+ mu 0.5))))
      (alpha [_ i]
        (do-if-int i #(+ 0)))
      (beta [_ i]
        (letfn [(func []
                  (if (odd? i)
                    (+ (half i) mu)
                    (half i)))]
          (do-if-int i func)))
      (w [_ x]
        (let [fact1 (pow (abs x) (twice mu))
              fact2 (exp (* (- x) x))]
          (* fact1 fact2))))
    (throw (IllegalArgumentException. "mu must be > than -0.5"))))

(defn jacobi [a b]
  (if (and (> a -1) (> b -1))
    (reify GaussOrthoPoly
      (mu-0 [_ ]
        (let [fact1 (+ a b 1)
              fact2-args [(inc a) (inc b) (+ a b 2)]
              fact2-terms (map #(Gamma/logGamma %) fact2-args)
              fact2 (- (reduce + fact2-terms)
                       (twice (last fact2-terms)))]
          (* (pow 2 fact1) (exp fact2))))
      (alpha [_ i]
        (do-if-int i #(alpha-jacobi a b i)))
      (beta [_ i]
        (do-if-int i #(beta-jacobi a b i)))
      (w [_ x]
        (* (pow (- 1 x) a) (pow (inc x) b))))
    (throw (IllegalArgumentException. "alpha & beta must be > than -1"))))

(defn legendre []
  (jacobi 0 0))

(defn chebyshev []
  (jacobi -0.5 -0.5))

(defn chebyshev2nd []
  (jacobi 0.5 0.5))

(defn gegenbauer [lambda]
  (jacobi (- lambda 0.5) (- lambda 0.5)))

(defn hyperbolic []
  (reify GaussOrthoPoly
    (mu-0 [_]
      Math/PI)
    (alpha [_ i]
      (do-if-int i #(+ 0)))
    (beta [_ i]
      (letfn [(func []
                (if (zero? i)
                  Math/PI
                  (* (sqr (half Math/PI)) (sqr i))))]
        (do-if-int i func)))
    (w [_ x]
      (/ 1 (Math/cosh x)))))

;;; The following fns are built on top of the fns implementing the GaussOrthogonalPolynomial
;;; protocol. They also take a GaussOrthoPoly object as first arg

(defn value [p n x]
  (let [e1 1
        e2  (- x (alpha p 0))]
    (letfn [(values
              ([]
                (concat [e1 e2] (values e1 e2 2)))
              ([a b n]
                (let [ newest (-  (* (- x (alpha p (dec n))) b)
                                  (* (beta p (dec n)) a))]
                  (lazy-seq 
                    (cons newest (values b newest (inc n)))))))]
    (nth (values) n))))

(defn weighted-val [p n x]
  (* (sqrt (w p x)) (value p n x)))

