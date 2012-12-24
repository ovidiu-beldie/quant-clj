(ns quant.common)

(defn sqr [x] (* x x))
(defn natural-nr? [x] (== x (Math/ceil x)))
(defn twice [x] (* 2 x))
(defn half [x]
  (if (and (natural-nr? x) (even? x))
    (bit-shift-right x 1)
    (/ x 2)))