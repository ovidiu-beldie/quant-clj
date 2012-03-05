(ns quant.test.math.test-matrix
  (:use
     [quant.math.matrix] 
     [clojure.test :only (deftest, deftest, is, testing)]))

(def m  [[1 2 3] [4 5 6] [7 8 9]])
(def m1 [[11 2 3] [41 5 6] [71 8 9]])
(def m2 [[1 2 3 4 5] [6 7 8 9 10] [11 12 13 14 15]])
(def m3  [[100 2 3] [4 101 6] [7 8 102]])

(deftest test-matrix
  (is (= m (matrix 3 3 (range 1 10))))
  (is (= m2 (matrix 3 5 (range 1 100)))))

(deftest test-column
  (is (= '(1 4 7) (column m 0)))
  (is (= '(2 5 8) (column m 1)))
  (is (= '(3 6 9) (column m 2))))

(deftest test-assoc-column
  (is (= m1 (assoc-column m [11 41 71] 0))))

(deftest test-set-main-diag
  (is (= m3 (set-main-diag m (range 100 200)))))

(def transp-m  [[1 4 7] [2 5 8] [3 6 9]])
(def transp-m2 [[1 6 11] [2 7 12] [3 8 13] [4 9 14] [5 10 15]])
(deftest test-transpose 
  (is (= transp-m (transpose m)))
  (is (= transp-m2 (transpose m2))))
