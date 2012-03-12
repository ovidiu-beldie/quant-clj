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
(def nb-vecs 50)
(def multiplier 100)
(def tolerance 1e-6)
(def seed 1)
(def accept-err 1e-11)
(def rng (MersenneTwister. seed))
(def random-series (repeatedly #(.nextInt rng)))
(def rs (map #(/ (abs %) Integer/MAX_VALUE) random-series))
(def test-matrix (matrix nb-vecs dim rs))
(def ordered-test-matrix (map vector test-matrix (range)))
(def projector (orthogonal-projections test-matrix multiplier tolerance))

(defn test1 []
  (let [pred-rows (fn[i j]
               (and ((projector :valid-vecs) j) (not (= i j))))
        comp-dot-prod (fn [r1 r2]
                        (reduce + (map * r1 r2)))
        pred-prods (fn [p]
                     (> (abs p) accept-err))]
    (loop [i 0, nb-errors 0]
      (if (= i nb-vecs)
        nb-errors
        (if ((projector :valid-vecs) i)
          (let [filtered-test-m (for [row ordered-test-matrix :when (pred-rows i (second row))] (first row))
                dot-prods (map (partial comp-dot-prod ((projector :vectors) i)) filtered-test-m)]
            (recur (inc i) (+ nb-errors (count (filter pred-prods dot-prods)))))
          (recur (inc i) nb-errors))))))

(defn test2 []
  (let [pred-rows (fn [i] ((projector :valid-vecs) i))
        filtered-test-m (for [row ordered-test-matrix :when (pred-rows (second row))] (first row))
        ordered-vecs (map vector (projector :vectors) (range))
        filtered-vecs (for [row ordered-vecs :when (pred-rows (second row))] (first row))
        inner-prod (fn [r1 r2] (reduce + (map * r1 r2)))
        in-prod-orig  (map inner-prod filtered-vecs filtered-test-m)
        norm-sq       (map inner-prod filtered-test-m filtered-test-m)
        pred-epsilon (fn [x y] (approx= x y accept-err))
        dif (map pred-epsilon in-prod-orig norm-sq)]
    (count (filter false? dif))))

(deftest test-test1
  (is (zero? (test1))))
         
(deftest test-test2
  (is (zero? (test2))))
            
