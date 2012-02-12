(ns quant.defines)

(defn epsilon-impl []
	(loop [e 1.0]
		(if (= (+ 1.0 (/ e 2.0)) 1.0)
			e
			(recur (/ e 2.0)))))

(def epsilon (memoize epsilon-impl))

(def min-pos-real (Double/MIN_VALUE))



