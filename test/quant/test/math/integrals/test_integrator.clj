; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software
; referenced in this library are the propriety of their respective owners

(ns quant.test.math.integrals.test-integrator
  (:use
   [quant.test.common :only (with-private-fns)]
   [incanter.core :only (sin cos)]
   [quant.math.integrals.core]
   [quant.common :only (sqr twice)]
   [clojure.algo.generic.math-functions :only (approx=)]
   [clojure.test :only (deftest, is, testing)]))

(def pi Math/PI)
(def tol 1.0e-6)  ;tolerance
(def max-iter 10000)
(def in-trapezoid            (partial integrate (trapezoid tol max-iter :default)))
(def in-simpson              (partial integrate (simpson tol max-iter)))
(def in-segment              (partial integrate (segment max-iter)))
(def in-kronrod-adaptive     (partial integrate (kronrod-adaptive tol 1000)))
(def in-kronrod-non-adaptive (partial integrate (kronrod-non-adaptive tol 100 tol)))

(with-private-fns [quant.math.integrals.trapezoid [default-integ]]
 (deftest test-default-integ
   (is (= 31/2 (default-integ #(twice %) 2 5 10 2)))
   (is (= 1915 (default-integ #(inc %) 50 100 30 3)))))
(with-private-fns [quant.math.integrals.trapezoid [midpoint-integ]]
 (deftest test-midpoint-integ
   (is (= 52/3 (midpoint-integ #(twice %) 2 5 10 2)))
   (is (= 7630/3 (midpoint-integ #(inc %) 50 100 30 3)))))

(def test-data
  [[1   (constantly 1) 0 1  "f(x)=1"]
   [1/2 identity       0 1  "f(x)=x"]
   [1/3 sqr            0 1  "f(x)=sqr(x)"]
   [2   sin            0 pi "f(x)=sin(x)"]
   [0   cos            0 pi "f(x)=cos(x)"]])

(defmacro gen-test [in-fn ex f a b desc]
  `(is (approx= ~ex ~(list in-fn f a b) tol) ~desc))

(defn all-tests [in-fn]
  (doseq [[ex f a b desc :as r] test-data]
    (gen-test in-fn ex f a b desc)))

(deftest test-integrate
  (testing "Trapezoid integral"
    (all-tests in-trapezoid))
  (testing "Simpson integral"
    (all-tests in-simpson))
  (testing "Segment integral"
    (all-tests in-segment))
  (testing "Kronrod adaptive integral"
    (all-tests in-kronrod-adaptive))
  (testing "Kronrod non adaptive integral"
    (all-tests in-kronrod-non-adaptive)))
