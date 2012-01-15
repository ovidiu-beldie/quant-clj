(ns quant.math.integrals.trapezoid)

(defstruct int-policy :integrate-fn :nb-evals)

(defn default-integrate [f a b I N]
	(let [dx (/ (- b a) N)
	  		val-x (+ a (/ dx 2))]
		(loop [sum 0, x val-x, i 0]
			(if (= i N)
				(-> (* dx sum)
					(+ I)
					(/ 2))
				(recur (+ sum (f x)) (+ x dx) (inc i))))))	
	
(def default-policy (struct int-policy default-integrate, 2))
	
(defn mid-point-integrate [f a b I N]
	(let [dx (/ (- b a) N)
	  		val-x (+ a (/ dx 6))
				D (* 2 dx (/ 1 3))]
		(loop [sum 0, x val-x, i 0]
			(if (= i N)
				(-> (* dx sum)
					(+ I)
					(/ 3))
				(recur (+ sum (f x) (f (+ x D))) (+ x dx) (inc i))))))


