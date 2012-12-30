; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.math.matrix.test-tqr-eigen-decomposition
  (:use
   [quant.test.common :only (with-private-fns)]
   [quant.math.matrix.tqr-eigen-decomposition]
   [clojure.algo.generic.math-functions :only (approx=)]
   [clojure.test :only (deftest, is, testing)]))

(def tol 1.0E-10)

(def d1 [0 1 2 3 4 5 6 7 8])
(def e1 [0 0 0 0 0 1 1 1 1])
(def e2 [0 10 20 30 40 50 60 70 80])
(def d [7 2 0 6 1 9 3 8 5])
(def ev [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [off-diag-zero?]]
 (deftest test-off-diag-zero?
   (is (true? (off-diag-zero? 1 d1 e1)))
   (is (false? (off-diag-zero? 6 d1 e1)))))

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [comp-q]]
 (deftest test-comp-q
   (is (= 3      (comp-q d1 e2 6 3 7 :no-shift)))
   (is (approx= -62.50208  (comp-q d1 e2 6 3 7 :close-eigen-value) 0.0001))
   (is (approx= -78.8776 (comp-q d1 e2 6 3 7 :over-relaxation) 0.0001))))


(def ev-result [[1 3.5 2.0 4] [5 9.5 4.0 8] [9 15.5 6.0 12] [13 21.5 8.0 16]])

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [update-ev]]
 (deftest test-update-ev
   (is (= ev-result (update-ev ev 2 0.5 1)))))

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [qr-transf-iter]]
 (def qti1 (qr-transf-iter 2 {:e e2, :ev ev, :sine 0.05, :cosine 0.025,
                              :d d, :u 0, :q 3, :l 1})))

(def e2-qr-1 [0 3.1622776601683795 20 30 40 50 60 70 80])
(def d-qr-1 [7 2.1 0 6 1 9 3 8 5])

(deftest test-qti1
  (is (= false (:recov-underflow qti1)))
  (is (approx= 0.3162277 (:sine qti1) 0.0001))
  (is (approx= 0.9486832 (:cosine qti1) 0.0001))
  (is (= d-qr-1 (:d qti1)))
  (is (approx= 0.1 (:u qti1) 0.0001))
  (is (= e2-qr-1 (:e qti1)))
  (is (approx= -0.2 (:q qti1) 0.0001)))

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [qr-transf-iter]]
 (def qti2 (qr-transf-iter 2 {:e e1, :ev ev, :sine 0.05, :cosine 0.025,
                              :d d, :u 9, :q 0, :l 1})))
(def d-qr-2 [7 -9 0 6 1 9 3 8 5])
(deftest test-qti2
  (is (= true (:recov-underflow qti2)))
  (is (= d-qr-2 (:d qti2)))
  (is (= e1 (:e qti2))))

(def d-sort [2 4 1 3])
(def d-sort-expect '(1 2 3 4))
(def ev-sort [[-1 2 3 -4] [5 -6 7 8] [9 10 -11 -12] [-13 -14 15 16]])
(def ev-sort-expect [[9 10 -11 -12] [1 -2 -3 4] [13 14 -15 -16] [5 -6 7 8]])

(with-private-fns [quant.math.matrix.tqr-eigen-decomposition [sort-eigens]]
 (def call-sort-eigens (sort-eigens {:ev ev-sort, :d d-sort})))

(deftest test-sort-eigens
  (is (= d-sort-expect (:d call-sort-eigens)))
  (is (= ev-sort-expect (:ev call-sort-eigens))))

(def d-val [11 7 6 2 0])

(def sub-val [1 1 1 1])

; I've adjusted these values as my computations 
; have better precision
(def ev-val-expect [11.246783221713914
                    7.485496736290855
                    5.5251516080277518
                    2.1811760273123308
                    -0.4386075933448487])
(def tqr-val (tqr-eigen-decomp d-val sub-val :without-eigen-vector))
(def ev-val-decomp (eigen-values tqr-val))

(deftest test-eigen-value
  (doseq [i (range (count ev-val-expect))]
   (is (approx= (ev-val-expect i) (ev-val-decomp i) tol))))

(def diag-vect [1 1])
(def sub-vect [1])
(def tqr-vect (tqr-eigen-decomp diag-vect sub-vect))
(def eigen-vect (eigen-vectors tqr-vect))

(deftest test-eigen-vector
  (is (< (+ 0.25 (reduce * (map #(apply * %) eigen-vect))) tol)))

(def diag-zero [12 9 6 3 0])
(def sub-zero-1 [0 1 0 1])
(def sub-zero-2 [1e-14 1 1e-14 1])
(def tqr-zero-1 (tqr-eigen-decomp diag-zero sub-zero-1))
(def tqr-zero-2 (tqr-eigen-decomp diag-zero sub-zero-2))
(def eigen-zero-val-comp (eigen-values tqr-zero-1))
(def eigen-zero-val-expect (eigen-values tqr-zero-2))

(deftest test-zero-off-diag
  (doseq [i (range (count eigen-zero-val-expect))]
   (is (approx= (eigen-zero-val-expect i) (eigen-zero-val-comp i) tol))))

