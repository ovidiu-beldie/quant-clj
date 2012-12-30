; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix.basis-incomplete-ordered
  (:use [quant.math.matrix.core :only (inner-prod)]))

(def tol 1e-12)

(defn basis-incomplete-ordered [euclidean-dim]
  "Creates a basis incomplete ordered obj with the provided euclidean dim and
with no vectors"
  {:euclid-dim euclidean-dim :curr-basis []})

(defn add-vector [{:keys [euclid-dim curr-basis] :as bio} v]
  "Adds a vector (2nd arg) to a basis incomplete ordered (1st arg)"
  (if (not (add-vector-valid? bio v))
    bio
    (let [inner-prods (map #(inner-prod % v 0) curr-basis)
          new-v-elem (fn [ip cb init]
                         (reduce init - (map #(* ip %) cb)))
          new-v (map new-v-elem inner-prods curr-basis v)
          norm (inner-prod new-v new-v 0)
          upd-fn (fn [cb] (conj cb (map #(/ % norm) new-v)))]
      (if (< norm tol)
        bio
        (update-in bio [:curr-basis] upd-fn)))))
          
(defn- add-vector-valid? [{:keys [euclid-dim curr-basis]} v]
  (if (not= (count v) euclid-dim)
    (throw (IllegalArgumentException. "invalid vector size"))
    (not= (count curr-basis) euclid-dim)))

