(ns quant.test.math.integrals.test-trapezoid
	(:use
		[quant.math.integrals.core]
		[quant.math.integrals.trapezoid]
		[clojure.contrib.generic.math-functions :only (approx=)]
		[clojure.test :only (deftest, deftest, is, testing)])) 

(def tolerance 1.0e-6)
(def max-iter 10000)
(defn sqr [x] (* x x))

(deftest test-default-integrate
	(is (= 31/2 (default-integrate #(* 2 %) 2 5 10 2)))
	(is (= 1915 (default-integrate #(+ 1 %) 50 100 30 3))))

(deftest test-mid-point-integrate
	(is (= 52/3 (mid-point-integrate #(* 2 %) 2 5 10 2)))
	(is (= 7630/3 (mid-point-integrate #(+ 1 %) 50 100 30 3))))

(deftest test-integrate
	(is (approx= 1/3 (integrate (trapezoid tolerance max-iter default-policy) sqr 0 1) tolerance))
	(is (approx= 1/3 (integrate (simpson tolerance max-iter) sqr 0 1) tolerance)))

