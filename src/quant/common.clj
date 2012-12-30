; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software
; referenced in this library are the propriety of their respective owners

(ns quant.common)

(defn sqr [x] (* x x))
(defn natural-nr? [x] (== x (Math/ceil x)))
(defn twice [x] (* 2 x))
(defn half [x]
  (try
    (if (even? x)
      (bit-shift-right x 1)
      (* x 0.5))
    (catch Exception e (* x 0.5))))