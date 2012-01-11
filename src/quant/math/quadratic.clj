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

