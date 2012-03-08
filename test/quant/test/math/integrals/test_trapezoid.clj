; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.math.integrals.test-trapezoid
	(:use
    [incanter.core :only (sin cos)]
    [incanter.distributions :only (normal-distribution)]
		[quant.math.integrals.core]
		[quant.math.integrals.trapezoid]
		[quant.math.integrals.segment]
		[quant.math.integrals.kronrod]
		[clojure.contrib.generic.math-functions :only (approx=)]
		[clojure.test :only (deftest, deftest, is, testing)])) 

(def tolerance 1.0e-6)
(def max-iter 10000)
(defn sqr [x] (* x x))
(defn const-1 [x] 1)
(defn id [x] x)
(def integrate-trapezoid            (partial integrate (trapezoid tolerance max-iter default-policy)))
(def integrate-simpson              (partial integrate (simpson tolerance max-iter)))
(def integrate-segment              (partial integrate (segment max-iter)))
(def integrate-kronrod-adaptive     (partial integrate (kronrod-adaptive tolerance 1000)))
(def integrate-kronrod-non-adaptive (partial integrate (kronrod-non-adaptive tolerance 100 tolerance)))

(deftest test-default-integrate
	(is (= 31/2 (default-integrate #(* 2 %) 2 5 10 2)))
	(is (= 1915 (default-integrate #(+ 1 %) 50 100 30 3))))

(deftest test-mid-point-integrate
	(is (= 52/3 (mid-point-integrate #(* 2 %) 2 5 10 2)))
	(is (= 7630/3 (mid-point-integrate #(+ 1 %) 50 100 30 3))))

(deftest test-integrate
  (testing "Trapezoid integral"
	  (is (approx= 1    (integrate-trapezoid const-1 0 1)   tolerance) "f(x)=1")
	  (is (approx= 1/2  (integrate-trapezoid id 0 1)        tolerance) "f(x)=x")
	  (is (approx= 1/3  (integrate-trapezoid sqr 0 1)       tolerance) "f(x)=sqr(x)")
	  (is (approx= 2    (integrate-trapezoid sin 0 Math/PI) tolerance) "f(x)=sin(x)")
	  (is (approx= 0    (integrate-trapezoid cos 0 Math/PI) tolerance) "f(x)=cos(x)"))
  (testing "Simpson integral"
	  (is (approx= 1    (integrate-simpson const-1 0 1)   tolerance) "f(x)=1")
	  (is (approx= 1/2  (integrate-simpson id 0 1)        tolerance) "f(x)=x")
	  (is (approx= 1/3  (integrate-simpson sqr 0 1)       tolerance) "f(x)=sqr(x)")
	  (is (approx= 2    (integrate-simpson sin 0 Math/PI) tolerance) "f(x)=sin(x)")
	  (is (approx= 0    (integrate-simpson cos 0 Math/PI) tolerance) "f(x)=cos(x)"))
  (testing "Segment integral"
	  (is (approx= 1    (integrate-segment const-1 0 1)   tolerance) "f(x)=1")
	  (is (approx= 1/2  (integrate-segment id 0 1)        tolerance) "f(x)=x")
	  (is (approx= 1/3  (integrate-segment sqr 0 1)       tolerance) "f(x)=sqr(x)")
	  (is (approx= 2    (integrate-segment sin 0 Math/PI) tolerance) "f(x)=sin(x)")
	  (is (approx= 0    (integrate-segment cos 0 Math/PI) tolerance) "f(x)=cos(x)")))
  (comment
  (testing "Kronrod adaptive integral"
	  (is (approx= 1    (integrate-kronrod-adaptive const-1 0 1)   tolerance) "f(x)=1")
	  (is (approx= 1/2  (integrate-kronrod-adaptive id 0 1)        tolerance) "f(x)=x")
	  (is (approx= 1/3  (integrate-kronrod-adaptive sqr 0 1)       tolerance) "f(x)=sqr(x)")
	  (is (approx= 2    (integrate-kronrod-adaptive sin 0 Math/PI) tolerance) "f(x)=sin(x)")
	  (is (approx= 0    (integrate-kronrod-adaptive cos 0 Math/PI) tolerance) "f(x)=cos(x)")))
  (comment
	(testing "Kronrod non adaptive integral"
	  (is (approx= 1    (integrate-kronrod-non-adaptive const-1 0 1)   tolerance) "f(x)=1")
	  (is (approx= 1/2  (integrate-kronrod-non-adaptive id 0 1)        tolerance) "f(x)=x")
	  (is (approx= 1/3  (integrate-kronrod-non-adaptive sqr 0 1)       tolerance) "f(x)=sqr(x)")
	  (is (approx= 2    (integrate-kronrod-non-adaptive sin 0 Math/PI) tolerance) "f(x)=sin(x)")
	  (is (approx= 0    (integrate-kronrod-non-adaptive cos 0 Math/PI) tolerance) "f(x)=cos(x)")))
