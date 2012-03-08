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
    [quant.math.matrix :only (matrix)])
  (:import [cern.jet.random.engine MersenneTwister]))

;(def dim 1000)
(def dim 3)
;(def nb-vecs 50)
(def nb-vecs 2)
;(def multiplier 100)
(def multiplier 10)
(def tolerance 1e-6)
(def seed 1)
(def accept-err 1e-11)
(def rng (MersenneTwister. seed))
(def random-series (repeatedly #(.nextInt rng)))
(def test-matrix (matrix nb-vecs dim random-series))
(def projector (orthogonal-projections test-matrix multiplier tolerance))





