; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.gauss-polynomials-impl
  (:import [cern.jet.stat.tdouble Gamma])
  (:use [clojure.contrib.math :only (ceil)]
        [clojure.contrib.generic.math-functions :only (sqr)]))

(declare handle-regular,handle-lhopital) 

(defn alpha-jacobi 
  ([a b i]
  "Compute limit operands for regular alpha"
    (let [numer (- (sqr b) (sqr a))
          factor (+ i i a b)
          denom (* factor (+ factor 2))]
      (handle-regular numer denom a b i alpha-jacobi)))

  ([a b i _]
  "Compute limit operands for l'Hopital alpha"
    (let [numer (* 2 b)
        denom (* 2 (+ a b i i 1))]
      (handle-lhopital numer denom))))
    
(defn beta-jacobi
  ([a b i]
  "Compute limit operands for regular beta"
    (let [numer (* 4 i (+ i a) (+ i b) (+ a b i))  
          factor (sqr (+ a b i i))
          denom (* factor (- factor 1))]
      (handle-regular numer denom a b i beta-jacobi)))

  ([a b i _]
  "Compute limit operands for l'Hopital beta" 
    (let [factor (+ i i a a b)
        numer (* 4 i (+ i b) factor)
        d (* 2 (+ i i a b))
        denom (* d (dec d))]
      (handle-lhopital numer denom))))

(defn handle-regular [numer denom a b i func]
  ""
  (if (zero? denom)
        (if (zero? numer)
          (func a b i :lhopital)
          (throw (ArithmeticException. "can't compute operand for jacobi integration")))
        (/ numer denom)))

(defn handle-lhopital [numer denom]
  ""
  (if (zero? denom)
        (throw (ArithmeticException. "can't compute operand for jacobi integration"))
        (/ numer denom)))

(defn do-if-int [i func]
  (if (= i (ceil i))
    (func)  
    (throw (IllegalArgumentException. "Parameter must be integer"))))
