(ns quant.test.math.integrals.gauss-polynomials.test-jacobi
	(:use 
		[quant.math.integrals.gauss-polynomials.jacobi]
		[clojure.test :only (deftest, deftest, is, testing)]))

(deftest test-compute
	(testing "Alpha" 
		(is (= 0 (compute :alpha [0 0] 0 :regular)) "Can be solved using l'Hopital")	
		(is (thrown? ArithmeticException (compute :alpha [1 2] -3/2 :regular)))	
		(is (= 3/99 (compute :alpha [1 2] 3 :regular))))
	(testing "Beta" 
		(is (thrown? ArithmeticException (compute :alpha [0 0] 0 :regular)))	
		(is (thrown? ArithmeticException (compute :alpha [1 2] -3/2 :regular)))	
		(is (= 3/99 (compute :alpha [1 2] 3 :regular)))))

(deftest test-do-if-params-valid
	(letfn [(func []
						(+ 1 1))]
		(is (= 2 (do-if-params-valid [0 0] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [-1 0] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [0 -1] func)))
		(is (thrown? IllegalArgumentException (do-if-params-valid [-1 -1] func)))))

(deftest test-lim-ops-a-r
	(is (= [0 0] (lim-ops-a-r [0 0] 0)))
	(is (= [0 8] (lim-ops-a-r [0 0] 1)))
	(is (= [3 99] (lim-ops-a-r [1 2] 3))))

(deftest test-lim-ops-b-r
	(is (= [0 0] (lim-ops-b-r [0 0] 0)))
	(is (= [4 12] (lim-ops-b-r [0 0] 1)))
	(is (= [1440 6480] (lim-ops-b-r [1 2] 3))))


(deftest test-lim-ops-a-h
	(is (= [0 2] (lim-ops-a-h [0 0] 0)))
	(is (= [0 6] (lim-ops-a-h [0 0] 1)))
	(is (= [4 20] (lim-ops-a-h [1 2] 3))))

(deftest test-lim-ops-b-h
	(is (= [0 0] (lim-ops-b-h [0 0] 0)))
	(is (= [8 12] (lim-ops-b-h [0 0] 1)))
	(is (= [600 306] (lim-ops-b-h [1 2] 3))))

(deftest test-process-limit
	(testing "Non-nul values"
		(is (= :ok (process-limit [1 2] :regular))) 
		(is (= :ok (process-limit [1 2] :lhopital))))
	(testing "Non-nul numerator, nul denominator"
		(is (= :fail (process-limit [1 0] :regular))) 
		(is (= :fail (process-limit [1 0] :lhopital))))
	(testing "Nul numerator, nul denominator"
		(is (= :lhopital (process-limit [0 0] :regular))) 
		(is (= :fail (process-limit [0 0] :lhopital)))))

(comment
(deftest all-tests
	(test-do-if-params-valid)
	(test-lim-ops-a-r)
	(test-lim-ops-b-r)
	(test-lim-ops-a-h)
	(test-lim-ops-b-h)
	(test-process-limit)))
