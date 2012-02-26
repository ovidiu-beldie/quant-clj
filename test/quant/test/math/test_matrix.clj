(ns quant.test.math.test-matrix
  (:use
     [quant.math.matrix] 
     [clojure.test :only (deftest, deftest, is, testing)]))

(def m  [[1 2 3] [4 5 6] [7 8 9]])
(def m1 [[11 2 3] [41 5 6] [71 8 9]])

(deftest test-column
  (is (= '(1 4 7) (column m 0)))
  (is (= '(2 5 8) (column m 1)))
  (is (= '(3 6 9) (column m 2))))

(deftest test-assoc-column
  (is (= m1 (assoc-column m [11 41 71] 0))))
