; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.matrix.basis-incomplete-ordered)

(defstruct basis-incomplete-ordered :current-basis :euclidean-dim)

(def tolerance 1e-12)

(defn basic-incomplete-ordered [euclidean-dim]
  (struct basis-incomplete-ordered [] euclidean-dim))

(defn add-vector-invariants-ok? [bio v]
  (if (not (= (count v) (bio :euclidean-dim)))
    (throw (IllegalArgumentException. "invalid vector size"))
    (not (= (count (bio ::current-basis)) (bio :euclidean-dim)))))

(defn inner-product [c1 c2 init]
  (reduce init + (map * c1 c2)))

(defn add-vector [bio v]
  (if (not (add-vector-invariants-ok? bio v))
    bio
    (let [curr-basis (bio :current-basis)
          inner-prods (map #(inner-product % v 0) curr-basis)
          new-v-elem (fn [ip cb init]
                         (reduce init - (map #(* ip %) cb)))
          new-v (map new-v-elem inner-prods curr-basis v)
          norm (inner-product new-v new-v 0)]
      (if (< norm tolerance)
        bio
        (assoc bio :current-basis (conj curr-basis (map #(/ % norm) new-v)))))))
          
    

