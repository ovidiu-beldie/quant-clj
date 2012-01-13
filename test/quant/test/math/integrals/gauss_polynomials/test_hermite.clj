(ns quant.test.math.integrals.gauss-polynomials.test-hermite
  (:use
    [quant.math.integrals.gauss-polynomials.hermite]
    [clojure.test :only (deftest, deftest, is, testing)]))

(deftest test-params-valid?
	(is (true? (params-valid? 0)))
	(is (true? (params-valid? -0.4)))
	(is (false? (params-valid? -0.6)))
	(is (false? (params-valid? -1))))

(deftest test-compute
  (testing "Alpha"
    (is (= 0 (compute :alpha 0 0)))
    (is (= 0 (compute :alpha 1 0)))
    (is (= 0 (compute :alpha 0 1))))
  (testing "Beta"
    (is (= 0 (compute :beta 0 0)))
    (is (= 1/2 (compute :beta 1 1)))
    (is (= 3 (compute :beta 2 2)))))

	
