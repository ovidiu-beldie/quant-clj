; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.math.matrix-utilities.test-orthogonal-projections
  (:use 
    [quant.math.matrix-utilities.orthogonal-projections]
    [quant.math.matrix :only (matrix)]
    [clojure.test :only (deftest, deftest, is, testing)]
    [clojure.contrib.generic.math-functions :only (approx=)]
    [incanter.core :only (abs)])
  (:import  [cern.jet.random.engine MersenneTwister]
            [java.lang Integer]))

(def dim 1000)
;(def dim 3)
(def nb-vecs 50)
;(def nb-vecs 3)
(def multiplier 100)
;(def multiplier 10)
(def tolerance 1e-6)
(def seed 1)
(def accept-err 1e-11)
(def rng (MersenneTwister. seed))
(def random-series (repeatedly #(.nextInt rng)))
(def rs (map #(/ (abs %) Integer/MAX_VALUE) random-series))
(def test-matrix (matrix nb-vecs dim rs))
;(def test-matrix (matrix nb-vecs dim (range)))
;(def test-matrix [[0.9094735984268941 0.5672659280557493 0.06925364586955572] 
;                  [0.21533483276857754 0.9950156812532878 0.17185625441924496] 
;                  [0.4932097198875666 0.012768743565664042 0.3805294634683661]])
(def ordered-test-matrix (map vector test-matrix (range)))
(def projector (orthogonal-projections test-matrix multiplier tolerance))

(defn test1 []
  (let [;_ (prn "vectors=" (projector :vectors))
        pred-rows (fn[i j]
               (and ((projector :valid-vecs) j) (not (= i j))))
        comp-dot-prod (fn [r1 r2]
                        (reduce + (map * r1 r2)))
        pred-prods (fn [p]
                     (> (abs p) accept-err))  
        ;_ (prn "OOOO ordered-test-matrix=" ordered-test-matrix)
        ]
    (loop [i 0, nb-errors 0]
      (if (= i nb-vecs)
        nb-errors
        (if ((projector :valid-vecs) i)
          (let [filtered-test-m (for [row ordered-test-matrix :when (pred-rows i (second row))] (first row))
                _ (prn "OOOOO filtered-test-m=" filtered-test-m "i=" i)
                _ (prn "OOOOO vectors=" (projector :vectors))
                ;_ (prn "OOOOO (vector i)=" ((projector :vectors) i))
                dot-prods (map (partial comp-dot-prod ((projector :vectors) i)) filtered-test-m)
                _ (prn "dot-prods=" dot-prods)
               ]
            (recur (inc i) (+ nb-errors (count (filter pred-prods dot-prods)))))
          (recur (inc i) nb-errors))))))

(defn test2 []
  (let [pred-rows (fn [i] ((projector :valid-vecs) i))
        filtered-test-m (for [row ordered-test-matrix :when (pred-rows (second row))] (first row))
        ordered-vecs (map vector (projector :vectors) (range))
        filtered-vecs (for [row ordered-vecs :when (pred-rows (second row))] (first row))
        inner-prod (fn [r1 r2]
                     (do
                     ;(prn "inner-prod: r1=" r1 "r2=" r2)
                     ;(prn "inner-prod ret=" (reduce + (map * r1 r2)))
                     (reduce + (map * r1 r2))))
        ;_ (prn "test2: filtered-test-m=" filtered-test-m "vectors=" (projector :vectors) "valids=" (projector :valid-vecs))
        in-prod-orig  (map inner-prod filtered-vecs filtered-test-m)
        norm-sq       (map inner-prod filtered-test-m filtered-test-m)
        pred-epsilon (fn [x y]
                       (approx= x y accept-err))
        dif (map pred-epsilon in-prod-orig norm-sq)
        ;dif (map #(abs (- %1 %2)) in-prod-orig norm-sq)
        ;_ (prn "in-prod-orig=" in-prod-orig "norm-sq=" norm-sq "dif=" dif)
        ;pred-pass-test (fn [d] (<= d accept-err))
        ]
    ;(count (filter pred-pass-test dif))))
    (count (filter false? dif))))

(deftest test-test1
  (is (zero? (test1))))
         
(deftest test-test2
  (is (zero? (test2))))
            



