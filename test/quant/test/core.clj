(ns quant.test.core
  (:use 
		[quant.core]
		[clojure.test :only (deftest)])
	(:require	
		[quant.math.integrals.gauss-polynomials.jacobi :as jacobi]))

(deftest all 
 	(jacobi/all-tests)) 
