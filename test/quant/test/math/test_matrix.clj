; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

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

(deftest test-get-main-diag
  (is (= '(1 5 9) (get-main-diag m)))
  (is (= '(100 101 102) (get-main-diag m3))))

(deftest test-set-main-diag
  (is (= m3 (set-main-diag m (range 100 200)))))

(def transp-m  [[1 4 7] [2 5 8] [3 6 9]])
(def transp-m2 [[1 6 11] [2 7 12] [3 8 13] [4 9 14] [5 10 15]])
(deftest test-transpose 
  (is (= transp-m (transpose m)))
  (is (= transp-m2 (transpose m2))))

(def count-m1 [[1 2] [3 4] [5 6] [7 8]])
(def count-m2 [[1 2 3 4] [5 6 7 8]])
(deftest test-count-rows
  (is (= 3 (count-rows m)))
  (is (= 4 (count-rows count-m1)))
  (is (= 2 (count-rows count-m2))))

(deftest test-count-cols
  (is (= 3 (count-cols m)))
  (is (= 2 (count-cols count-m1)))
  (is (= 4 (count-cols count-m2))))

(deftest test-matrix?
  (is (true? (matrix? m)))
  (is (false? (matrix? ['(1 2 3) '(4 5 6) '(7 8 9)])))
  (is (false? (matrix? (list [1 2 3] [4 5 6] [7 8 9]))))
  (is (false? (matrix? [1 2 3 4])))
  (is (false? (matrix? 4))))

(def mx2  [[2 4 6] [8 10 12] [14 16 18]])
(def mm1 (matrix 3 4 (range 1 13)))
(def mm2 (matrix 4 2 (range 9 1 -1)))
(def a1 [3 9])
(def a2 [1 7 4])
(def a3 [6 8 5 0])

(def mm1xmm2 [[50 40] [146 120] [242 200]])

(deftest test-multiply
  (testing "Scalar"
    (is (= mx2 (multiply m 2)))
    (is (= mx2 (multiply 2 m))))
  (testing "Matrices"
    (is (= mm1xmm2 (multiply mm1 mm2)))
    (is (thrown? IllegalArgumentException (multiply mm2 mm1))))
  (testing "Vectors"
    (is (thrown? IllegalArgumentException (multiply mm1 a1)))
    (is (thrown? IllegalArgumentException (multiply mm1 a2)))
    (is (= [37 113 189] (multiply mm1 a3)))
    (is (= [99 75 51 27] (multiply mm2 a1)))
    (is (thrown? IllegalArgumentException (multiply mm2 a2)))
    (is (thrown? IllegalArgumentException (multiply mm2 a3)))
    (is (thrown? IllegalArgumentException (multiply a1 mm1)))
    (is (= [72 84 96 108] (multiply a2 mm1)))
    (is (thrown? IllegalArgumentException (multiply a3 mm1)))
    (is (thrown? IllegalArgumentException (multiply a1 mm2)))
    (is (thrown? IllegalArgumentException (multiply a2 mm2)))
    (is (= [135 116] (multiply a3 mm2)))))
