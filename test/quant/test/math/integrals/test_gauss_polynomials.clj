(ns quant.test.math.integrals.test-gauss-polynomials
	(:use
		[quant.math.integrals.gauss-polynomials]
		[clojure.test :only (deftest, deftest, is, testing)]))

;;; Constructors

(deftest test-laguerre
	(is (satisfies? GaussOrthogonalPolynomial (laguerre 0)))
	(is (satisfies? GaussOrthogonalPolynomial (laguerre -0.5)))
	(is (thrown? IllegalArgumentException (laguerre -1)))
	(is (thrown? IllegalArgumentException (laguerre -100))))

(deftest test-hermite
	(is (satisfies? GaussOrthogonalPolynomial (hermite 0)))
	(is (satisfies? GaussOrthogonalPolynomial (hermite -0.3)))
	(is (thrown? IllegalArgumentException (hermite -0.7)))
	(is (thrown? IllegalArgumentException (hermite -10))))

(deftest test-jacobi
	(is (satisfies? GaussOrthogonalPolynomial (jacobi 0 0)))
	(is (thrown? IllegalArgumentException (jacobi -1 0)))
	(is (thrown? IllegalArgumentException (jacobi 100 -100)))
	(is (thrown? IllegalArgumentException (jacobi -1 -1))))

;;; Alpha

(deftest test-alpha
	(testing "Non-integer parameter"
		(is (thrown? IllegalArgumentException (alpha (jacobi 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (alpha (laguerre 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (alpha (hermite 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (alpha (hyperbolic 1 2) 5.3))))
	(testing "Laguerre"
		(is (= 14.9 (alpha (laguerre -0.1) 7))))	
	(testing "Hermite"
		(is (= 0  (alpha (hermite 1.23) 20))))
	(testing "Jacobi"
		(is (= 0 (alpha (jacobi 0 0) 0)) "Can be solved using l'Hopital")	
		(is (= 3/99 (alpha (jacobi 1 2) 3)))))

(deftest test-alpha-jacobi-lhopital
	(testing "L'Hopital form"
		(is (= 0 (alpha-impl 0 0 0 :lhopital)))
		(is (= 0 (alpha-impl 0 0 1 :lhopital)))
		(is (= 4/20 (alpha-impl 1 2 3 :lhopital)))))

;;; Beta

(deftest test-beta
	(testing "Non-integer parameter"
		(is (thrown? IllegalArgumentException (beta (jacobi 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (beta (laguerre 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (beta (hermite 1 2) 5.3)))
		(is (thrown? IllegalArgumentException (beta (hyperbolic 1 2) 5.3))))
	(testing "Laguerre"
		(is (= (double 5) (beta (laguerre 0.5) 2))))
	(testing "Hermite"
		(is (= 7/2 (beta (hermite 2) 3)))
		(is (= 4 (beta (hermite 1) 8))))
	(testing "Jacobi"
		(is (thrown? ArithmeticException (beta (jacobi 0 0) 0)))	
		(is (= 2/9 (beta (jacobi 1 2) 3)))))

(deftest test-beta-jacobi-lhopital
	(testing "L'Hopital form"
		(is (thrown? ArithmeticException (beta-impl 0 0 0 :lhopital)))
		(is (= 8/12 (beta-impl 0 0 1 :lhopital)))
		(is (= 600/306 (beta-impl 1 2 3 :lhopital)))))
