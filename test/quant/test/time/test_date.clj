; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.time.test-date
  (:use
    [quant.time.date :only (new-date)]
    [clojure.test :only (deftest, deftest, is, testing)]
    [quant.time.date]))

(def d1 (new-date 31 1 1987))
(def d2 (new-date 28 2 1991))
(def d3 (new-date 29 2 1992))
(def d4 (new-date 31 3 1994))
(def d5 (new-date 30 4 1981))
(def d6 (new-date 31 5 1981))
(def d7 (new-date 30 6 1981))
(def d8 (new-date 31 7 1981))
(def d9 (new-date 31 8 1981))
(def d10 (new-date 30 9 1981))
(def d11 (new-date 31 10 1981))
(def d12 (new-date 30 11 1981))
(def d13 (new-date 31 12 1981))

(def dd1 (new-date 3 1 1987))
(def dd2 (new-date 8 2 1991))
(def dd3 (new-date 9 2 1992))
(def dd4 (new-date 1 3 1994))
(def dd5 (new-date 3 4 1981))
(def dd6 (new-date 1 5 1981))
(def dd7 (new-date 3 6 1981))
(def dd8 (new-date 1 7 1981))
(def dd9 (new-date 3 8 1981))
(def dd10 (new-date 3 9 1981))
(def dd11 (new-date 3 10 1981))
(def dd12 (new-date 3 11 1981))
(def dd13 (new-date 1 12 1981))


(deftest test-end-of-month
  (is (true? (end-of-month? d1)))
  (is (true? (end-of-month? d2)))
  (is (true? (end-of-month? d3)))
  (is (true? (end-of-month? d4)))
  (is (true? (end-of-month? d5)))
  (is (true? (end-of-month? d6)))
  (is (true? (end-of-month? d7)))
  (is (true? (end-of-month? d8)))
  (is (true? (end-of-month? d9)))
  (is (true? (end-of-month? d10)))
  (is (true? (end-of-month? d11)))
  (is (true? (end-of-month? d12)))
  (is (true? (end-of-month? d13)))


  (is (false? (end-of-month? dd1)))
  (is (false? (end-of-month? dd2)))
  (is (false? (end-of-month? dd3)))
  (is (false? (end-of-month? dd4)))
  (is (false? (end-of-month? dd5)))
  (is (false? (end-of-month? dd6)))
  (is (false? (end-of-month? dd7)))
  (is (false? (end-of-month? dd8)))
  (is (false? (end-of-month? dd9)))
  (is (false? (end-of-month? dd10)))
  (is (false? (end-of-month? dd11)))
  (is (false? (end-of-month? dd12)))
  (is (false? (end-of-month? dd13))))
  
