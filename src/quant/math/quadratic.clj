; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.quadratic
	(:use 
		[incanter.core :only (sqrt)]))


(defn turn-point [a b c]
	""
	(/ (- b) (* a 2)))


(defn- discriminant [a b c]
	""
	(- (* b b) (* 4 a c)))


(def discrim (memoize discriminant))


(defn roots-real? [a b c]
	""
	(> (discrim a b c) 0))


(defn roots [a b c]
	""
	(if (roots-real? a b c)
		(let [disc (sqrt (discrim a b c))
					denom (* 2 a)
					num1 (- (- b) disc)
					num2 (+ (- b) disc)]
			(prn "num1: " num1 ", denom:" denom ", disc: " disc ", discrim: " (discrim a b c))
			(list (/ num1 denom) (/ num2 denom)))
		(repeat 2 (turn-point a b c))))

