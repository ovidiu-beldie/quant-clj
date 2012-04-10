; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.time.calendars.test-europe
  (:use 
    [quant.time.date :only (new-date day)]
    [quant.time.calendar :only (holiday-list new-calendar)]
    [clojure.test :only (deftest, deftest, is, testing)]
    [quant.time.calendars.europe]))

(def d1 (new-date 1 1 2003))
(def d2 (new-date 18 4 2003))
(def d3 (new-date 21 4 2003))
(def d4 (new-date 1 5 2003))
(def d5 (new-date 24 12 2003))
(def d6 (new-date 25 12 2003))
(def d7 (new-date 26 12 2003))
(def d8 (new-date 31 12 2003))

(def d11 (new-date 1 1 2004))
(def d12 (new-date 9 4 2004))
(def d13 (new-date 12 4 2004))
(def d14 (new-date 24 12 2004))
(def d15 (new-date 31 12 2004))

;;; Testing invalid

(deftest test-create-calendar
  (is (thrown? IllegalArgumentException (new-calendar :andorra :eurex)))
  (is (thrown? IllegalArgumentException (new-calendar :germany :unknown))))

;;; Germany

;; Frankfurt
(def frank-calendar (new-calendar :germany :frankfurt-stock-exchange))
(def frank-holiday-list (holiday-list frank-calendar (new-date 1 1 2003) (new-date 31 12 2004)))
(def frank-expected-list (list d1 d2 d3 d4 d5 d6 d7 d8 d11 d12 d13 d14 d15))

(deftest test-holiday-list-frankfurt
  (testing "Frankfurt Stock Exchange"
    (is (= (sort frank-holiday-list) (sort frank-expected-list)))
    (is (= "Frankfurt stock exchange" (frank-calendar :name)))))

;; Eurex 
(def eurex-calendar (new-calendar :germany :eurex))
(def eurex-holiday-list (holiday-list eurex-calendar (new-date 1 1 2003) (new-date 31 12 2004)))
(def eurex-expected-list (list d1 d2 d3 d4 d5 d6 d7 d8 d11 d12 d13 d14 d15))

(deftest test-holiday-list-eurex
  (testing "Eurex"
    (is (= (sort eurex-holiday-list) (sort eurex-expected-list)))
    (is (= "Eurex" (eurex-calendar :name)))))

