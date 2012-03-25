(ns quant.test.math.matrix-utilities.test-symmetric-schur-decomposition
  (:use 
    [quant.math.matrix-utilities.symmetric-schur-decomposition]
    [clojure.test :only (deftest, is, testing)]))

(def null-matrix [])
(def rectang-matrix [[1 2] [3 4] [5 6]])
(def square-matrix [[1 2 3] [4 5 6] [7 8 9]])

(deftest test-symmetric-schur-decomp
  (is (thrown? IllegalArgumentException (symmetric-schur-decomp null-matrix)))
  (is (thrown? IllegalArgumentException (symmetric-schur-decomp rectang-matrix))))

(def m1 [[1 2] [3 4]])
(def m2 [[1 2 3] [4 5 6] [7 8 9]])
(def m3 [[7.9 -2.6 3.1] [6.9 -0.3 2.5] [-1.7 -5.8 3.4]])

(def sym-schur-decomp-1 (symmetric-schur-decomp m1))
(def sym-schur-decomp-2 (symmetric-schur-decomp m2))
(def sym-schur-decomp-3 (symmetric-schur-decomp m3))

(deftest test-symmetric-schur-decomp-1
  (is (= [5.0 0] (sym-schur-decomp-1 :diag)))
  (is (= [[0.4472135954999579 0.8944271909999159] [0.8944271909999159 -0.4472135954999579]] (sym-schur-decomp-1 :eigen-vecs))))
  
(deftest test-symmetric-schur-decomp-2
  (is (= [14.300735266508607 0.6992647415727308 -8.081337865857956E-9] (sym-schur-decomp-2 :diag)))
  (is (= [[0.2614963974210536 0.17781911950263637 0.9486832953494636] 
          [0.562313387008296 -0.8269242134508205 1.1180691182356011E-8] 
          [0.7844891898089282 0.5334573140824381 -0.31622777411998837]] (sym-schur-decomp-2 :eigen-vecs))))

(deftest test-symmetric-schur-decomp-3
  (is (= [9.64526589301084 4.237647485018301 -2.882913378029142] (sym-schur-decomp-3 :diag)))
  (is (= [[0.9080435819324038 0.2521963531323654 0.3344455901609486] 
          [-0.1379713387661744 -0.5738007998122164 0.8072896331639161] 
          [0.39550064853352573 -0.7791980759740079 -0.4862402651035439]] (sym-schur-decomp-3 :eigen-vecs))))
