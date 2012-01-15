(ns quant.test.math.integrals.test-gauss-polynomials
	(:use
		[quant.math.integrals.gauss-polynomials]
		[clojure.test :only (deftest, deftest, is, testing)]))

;;; Constructors

(deftest test-laguerre
	(is (satisfies? GaussOrthogonalPolynomial (laguerre 0)))
	(is (satisfies? GaussOrthogonalPolynomial (laguerre -0.5)))
	(is (thrown-with-msg? IllegalArgumentException #"s must be superior to -1" (laguerre -1)))
	(is (thrown-with-msg? IllegalArgumentException #"s must be superior to -1" (laguerre -100))))

(deftest test-hermite
	(is (satisfies? GaussOrthogonalPolynomial (hermite 0)))
	(is (satisfies? GaussOrthogonalPolynomial (hermite -0.3)))
	(is (thrown-with-msg? IllegalArgumentException #"mu must be superior to -0.5" (hermite -0.7)))
	(is (thrown-with-msg? IllegalArgumentException #"mu must be superior to -0.5" (hermite -10))))

(deftest test-jacobi
	(is (satisfies? GaussOrthogonalPolynomial (jacobi 0 0)))
	(is (thrown-with-msg? IllegalArgumentException #"Both alpha and beta must be superior to -1" (jacobi -1 0)))
	(is (thrown-with-msg? IllegalArgumentException #"Both alpha and beta must be superior to -1" (jacobi 100 -100)))
	(is (thrown-with-msg? IllegalArgumentException #"Both alpha and beta must be superior to -1" (jacobi -1 -1))))

;;; alpha

(deftest test-alpha
	(testing "Non-integer parameter"
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (alpha (jacobi 1 2) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (alpha (laguerre 2) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (alpha (hermite 1) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (alpha (hyperbolic) 5.3))))

	(testing "Laguerre"
		(is (= 14.9 (alpha (laguerre -0.1) 7))))	

	(testing "Hermite"
		(is (= 0  (alpha (hermite 1.23) 20))))

	(testing "Jacobi"
		(is (= 0 (alpha (jacobi 0 0) 0)) "Can be solved using l'Hopital")	
		(is (= 3/99 (alpha (jacobi 1 2) 3)))))

(deftest test-alpha-jacobi-lhopital
	(testing "L'Hopital form"
		(is (= 0 (alpha-jacobi 0 0 0 :lhopital)))
		(is (= 0 (alpha-jacobi 0 0 1 :lhopital)))
		(is (= 4/20 (alpha-jacobi 1 2 3 :lhopital)))))

;;; Beta

(deftest test-beta
	(testing "Non-integer parameter"
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (beta (jacobi 1 2) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (beta (laguerre 2) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (beta (hermite 1) 5.3)))
		(is (thrown-with-msg? IllegalArgumentException #"Parameter must be integer" (beta (hyperbolic) 5.3))))

	(testing "Laguerre"
		(is (= (double 5) (beta (laguerre 0.5) 2))))

	(testing "Hermite"
		(is (= 7/2 (beta (hermite 2) 3)))
	 	(is (= 4 (beta (hermite 1) 8))))

	(testing "Jacobi"
		(is (thrown-with-msg? ArithmeticException #"can't compute operand for jacobi integration" (beta (jacobi 0 0) 0)))	
		(is (= 2/9 (beta (jacobi 1 2) 3)))))

(deftest test-beta-jacobi-lhopital
	(testing "L'Hopital form"
		(is (thrown-with-msg? ArithmeticException #"can't compute operand for jacobi integration" (beta-jacobi 0 0 0 :lhopital)))
		(is (= 8/12 (beta-jacobi 0 0 1 :lhopital)))
		(is (= 600/306 (beta-jacobi 1 2 3 :lhopital)))))
