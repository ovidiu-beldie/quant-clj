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
	
