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

(deftest test-daycount
  (testing "BondBasis/USA"
    (is (= 4735   (day-count d1 d2 :bond-basis)))
    (is (= 7288   (day-count d1 d3 :usa)))
    (is (= 13797  (day-count d1 d4 :bond-basis)))
    (is (= 9062   (day-count d2 d4 :usa)))
    (is (= 7260   (day-count d3 d5 :bond-basis)))
    (is (= 11673  (day-count d2 d6 :usa)))
    (is (= 4060   (day-count d5 d7 :bond-basis)))
    (is (= 17533  (day-count d2 d8 :usa)))
    (is (= 8907   (day-count d5 d9 :bond-basis)))
    (is (= 18719 (day-count d2 d9 :bond-basis)))
    (is (= 20160 (day-count d2 d10 :bond-basis))))
  (testing "European/EurobondBasis"
    (is (= 4735   (day-count d1 d2 :european)))
    (is (= 7287   (day-count d1 d3 :eurobond-basis)))
    (is (= 13797  (day-count d1 d4 :european)))
    (is (= 9062   (day-count d2 d4 :eurobond-basis)))
    (is (= 7260   (day-count d3 d5 :european)))
    (is (= 11672  (day-count d2 d6 :eurobond-basis)))
    (is (= 4060   (day-count d5 d7 :european)))
    (is (= 17533  (day-count d2 d8 :eurobond-basis)))
    (is (= 8907   (day-count d5 d9 :european)))
    (is (= 18719   (day-count d2 d9 :eurobond-basis)))
    (is (= 20160   (day-count d2 d10 :european))))
  (testing "Italian"
    (is (= 4737   (day-count d1 d2 :italian)))
    (is (= 7287   (day-count d1 d3 :italian)))
    (is (= 13797  (day-count d1 d4 :italian)))
    (is (= 9060   (day-count d2 d4 :italian)))
    (is (= 7260   (day-count d3 d5 :italian)))
    (is (= 11670  (day-count d2 d6 :italian)))
    (is (= 4060   (day-count d5 d7 :italian)))
    (is (= 17531  (day-count d2 d8 :italian)))
    (is (= 8907   (day-count d5 d9 :italian)))
    (is (= 18717   (day-count d2 d9 :italian)))
    (is (= 20160   (day-count d2 d10 :italian)))))

