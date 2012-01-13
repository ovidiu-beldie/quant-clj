(ns quant.test.math.integrals.gauss-polynomials.test-jacobi
	(:use
		[quant.math.integrals.gauss-polynomials.core 
			:only (GaussOrthoPolyProtocol, compute-alpha, compute-beta)] 
		[quant.math.integrals.gauss-polynomials.jacobi]
		[clojure.test :only (deftest, deftest, is, testing)]))

(deftest test-jacobi
		(is (satisfies? GaussOrthoPolyProtocol (jacobi 0 0)))
		(is (thrown? IllegalArgumentException (jacobi -1 0)))
		(is (thrown? IllegalArgumentException (jacobi 100 -100)))
		(is (thrown? IllegalArgumentException (jacobi -1 -1))))

(deftest test-compute-alpha
	(is (= 0 (compute-alpha (jacobi 0 0) 0)) "Can be solved using l'Hopital")	
	(is (thrown? ArithmeticException (compute-alpha (jacobi 1 2) -3/2)))	
	(is (= 3/99 (compute-alpha (jacobi 1 2) 3))))

(deftest test-compute-beta
	(is (thrown? ArithmeticException (compute-beta (jacobi 0 0) 0)))	
	(is (thrown? ArithmeticException (compute-beta (jacobi 1 2) -3/2)))	
	(is (= 2/9 (compute-beta (jacobi 1 2) 3))))

(deftest test-alpha-impl
	(testing "Regular form"
		(is (= 0 (alpha-impl (jacobi 0 0) 0)) "Can be solved using l'Hopital")
		(is (= 0 (alpha-impl (jacobi 0 0) 1)))
		(is (= 3/99 (alpha-impl (jacobi 1 2) 3))))
	(testing "L'Hopital form"
		(is (= 0 (alpha-impl 0 0 0 :lhopital)))
		(is (= 0 (alpha-impl 0 0 1 :lhopital)))
		(is (= 4/20 (alpha-impl 1 2 3 :lhopital)))))

(deftest test-beta-impl
	(testing "Regular form"
		(is (thrown? ArithmeticException (beta-impl (jacobi 0 0) 0)))
		(is (= 4/12 (beta-impl (jacobi 0 0) 1)))
		(is (= 1440/6480 (beta-impl (jacobi 1 2) 3))))
	(testing "L'Hopital form"
		(is (thrown? ArithmeticException (beta-impl 0 0 0 :lhopital)))
		(is (= 8/12 (beta-impl 0 0 1 :lhopital)))
		(is (= 600/306 (beta-impl 1 2 3 :lhopital)))))


