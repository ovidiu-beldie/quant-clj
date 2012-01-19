(ns quant.math.integrals.trapezoid
	(:use   
		[quant.math.integrals.core]
		[incanter.core :only (abs)]
		[clojure.stacktrace]))

(defstruct integration-policy :policy-integrate :policy-nb-evals)

(defn default-integrate [f a b I N]
	(let [dx (/ (- b a) N)
	  		val-x (+ a (/ dx 2))]
		(loop [sum 0, x val-x, i 0]
			(if (= i N)
				(-> (* dx sum)
					(+ I)
					(/ 2))
				(recur (+ sum (f x)) (+ x dx) (inc i))))))	
	
(def default-policy (struct integration-policy default-integrate, 2))
	
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

(defn done? [x y z i]
	(and (<= (abs (- x y)) z)
				(> i 5)))

(defn init-i [f a b]
	(* (+ (f a) (f b)) 
			(- b a) 
			(double (/ 1 2))))

(defrecord Trapezoid [in])

(extend-type Trapezoid
	Integrable

	(integrate [{:keys [in]} f a b]
		(let [integ (partial (in :policy-integrate) f a b)]
			(loop [N 1, I (init-i f a b), i 1]
				(let [newI (integ I N)
							_ (prn "I=" I "newI=" newI)]
					(if (done? I newI (in :abs-accuracy) i)
						newI
						(if (= i (in :max-evals))
							(throw (Error. "max number of iterations reached"))
							(recur (* N (in :policy-nb-evals)) newI (inc i))))))))) 

(defn trapezoid [abs-accuracy max-evals integ-policy]
	(let [arg (make-integrator abs-accuracy max-evals)]
		(Trapezoid. (merge arg integ-policy))))

(defrecord Simpson [in])

(extend-type Simpson
	Integrable

	(integrate [{:keys [in]} f a b]
		(let [integ (partial (in :policy-integrate) f a b)]
			(loop [N 1, I (init-i f a b), adjI (init-i f a b), i 1]
				(let [newI (integ I N)
							newAdjI (-> newI
												(* 4)
												(- I)
												(/ 3))]
					(if (done? adjI newAdjI (in :abs-accuracy) i)
						newAdjI
						(if (= i (in :max-evals))
							(throw (Error. "max number of iterations reached"))
							(recur (* N 2) newI newAdjI (inc i)))))))))

(defn simpson [abs-accuracy max-evals]
	(let [arg (make-integrator abs-accuracy max-evals)]
		(Trapezoid. (merge arg default-policy))))
