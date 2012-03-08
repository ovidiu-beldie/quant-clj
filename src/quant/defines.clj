; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.defines)

(defn epsilon-impl []
	(loop [e 1.0]
		(if (= (+ 1.0 (/ e 2.0)) 1.0)
			e
			(recur (/ e 2.0)))))

(def epsilon (memoize epsilon-impl))

(def min-pos-real (Double/MIN_VALUE))



