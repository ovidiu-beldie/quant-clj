; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.time.daycounters.test-thirty360
  (:use 
    [quant.time.date :only (new-date)]
    [clojure.test :only (deftest, deftest, is, testing)]
    [quant.time.daycounter]
    [quant.time.daycounters.thirty360]))

(def d1 (new-date 3 1 1943))
(def d2 (new-date 28 2 1956))
(def d3 (new-date 31 3 1963))
(def d4 (new-date 30 4 1981))
(def d5 (new-date 31 5 1983))
(def d6 (new-date 31 7 1988))
(def d7 (new-date 10 9 1994))
(def d8 (new-date 11 11 2004))
(def d9 (new-date 27 2 2008))
(def d10 (new-date 28 2 2012))

(def us-30 (new-thirty360))
(def eu-30 (new-thirty360 :european))
(def it-30 (new-thirty360 :italian))

(deftest test-daycount
  (testing "BondBasis/USA"
    (is (= "30/360 (Bond Basis)" (get-name us-30)))
    (is (= 4735   (day-count us-30 d1 d2)))
    (is (= 7288   (day-count us-30 d1 d3)))
    (is (= 13797  (day-count us-30 d1 d4)))
    (is (= 9062   (day-count us-30 d2 d4)))
    (is (= 7260   (day-count us-30 d3 d5)))
    (is (= 11673  (day-count us-30 d2 d6)))
    (is (= 4060   (day-count us-30 d5 d7)))
    (is (= 17533  (day-count us-30 d2 d8)))
    (is (= 8907   (day-count us-30 d5 d9)))
    (is (= 18719  (day-count us-30 d2 d9)))
    (is (= 20160  (day-count us-30 d2 d10))))
  (testing "European/EurobondBasis"
    (is (= "30E/360 (Eurobond Basis)" (get-name eu-30)))
    (is (= 4735   (day-count eu-30 d1 d2)))
    (is (= 7287   (day-count eu-30 d1 d3)))
    (is (= 13797  (day-count eu-30 d1 d4)))
    (is (= 9062   (day-count eu-30 d2 d4)))
    (is (= 7260   (day-count eu-30 d3 d5)))
    (is (= 11672  (day-count eu-30 d2 d6)))
    (is (= 4060   (day-count eu-30 d5 d7)))
    (is (= 17533  (day-count eu-30 d2 d8)))
    (is (= 8907   (day-count eu-30 d5 d9)))
    (is (= 18719  (day-count eu-30 d2 d9)))
    (is (= 20160  (day-count eu-30 d2 d10))))
  (testing "Italian"
    (is (= "30/360 30/360 (Italian)" (get-name it-30)))
    (is (= 4737   (day-count it-30 d1 d2)))
    (is (= 7287   (day-count it-30 d1 d3)))
    (is (= 13797  (day-count it-30 d1 d4)))
    (is (= 9060   (day-count it-30 d2 d4)))
    (is (= 7260   (day-count it-30 d3 d5)))
    (is (= 11670  (day-count it-30 d2 d6)))
    (is (= 4060   (day-count it-30 d5 d7)))
    (is (= 17531  (day-count it-30 d2 d8)))
    (is (= 8907   (day-count it-30 d5 d9)))
    (is (= 18717  (day-count it-30 d2 d9)))
    (is (= 20160  (day-count it-30 d2 d10)))))

