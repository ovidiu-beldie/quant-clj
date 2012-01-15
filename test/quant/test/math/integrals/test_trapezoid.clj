(ns quant.test.math.integrals.test-trapezoid
	(:use
		[quant.math.integrals.trapezoid]
		[clojure.test :only (deftest, deftest, is, testing)])) 

(deftest test-default-integrate
	(is (= 31/2 (default-integrate #(* 2 %) 2 5 10 2)))
	(is (= 1915 (default-integrate #(+ 1 %) 50 100 30 3))))

(deftest test-mid-point-integrate
	(is (= 52/3 (mid-point-integrate #(* 2 %) 2 5 10 2)))
	(is (= 7630/3 (mid-point-integrate #(+ 1 %) 50 100 30 3))))
