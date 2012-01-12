(ns quant.test.math.integrals.gauss-polynomials.test-laguerre
  (:use
    [quant.math.integrals.gauss-polynomials.laguerre]
    [clojure.test :only (deftest, deftest, is, testing)]))

(deftest test-params-valid?
	(is (true? (params-valid? 0)))
	(is (true? (params-valid? -0.5)))
	(is (false? (params-valid? -1)))
	(is (false? (params-valid? -1.5))))

(deftest test-compute
  (testing "Alpha"
    (is (= 1 (compute :alpha 0 0)))
    (is (= 2 (compute :alpha 1 0)))
    (is (= 3 (compute :alpha 0 1))))
  (testing "Beta"
    (is (= 0 (compute :beta 0 0)))
    (is (= 0 (compute :beta 1 0)))
    (is (= 1 (compute :beta 0 1)))))

	
