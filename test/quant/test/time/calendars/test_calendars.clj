; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.test.time.calendars.test-calendars
  (:use 
    [quant.time.date :only (new-date, day, month, year)]
    [quant.time.calendar :only (holiday-list, new-calendar, business-day?)]
    [clojure.test :only (deftest, deftest, is, testing)]))

;;; Testing invalid

(deftest test-create-calendar
  (is (thrown? IllegalArgumentException (new-calendar :andorra :eurex)))
  (is (thrown? IllegalArgumentException (new-calendar :germany :unknown))))


;;; Germany

(def d1 (new-date  1  1 2003))
(def d2 (new-date 18  4 2003))
(def d3 (new-date 21  4 2003))
(def d4 (new-date  1  5 2003))
(def d5 (new-date 24 12 2003))
(def d6 (new-date 25 12 2003))
(def d7 (new-date 26 12 2003))
(def d8 (new-date 31 12 2003))

(def d11 (new-date  1  1 2004))
(def d12 (new-date  9  4 2004))
(def d13 (new-date 12  4 2004))
(def d14 (new-date 24 12 2004))
(def d15 (new-date 31 12 2004))

;; Frankfurt
(def frank-calendar (new-calendar :germany :frankfurt-stock-exchange))
(def frank-holiday-list (holiday-list frank-calendar (new-date 1 1 2003) (new-date 31 12 2004)))
(def frank-expected-list (list d1 d2 d3 d4 d5 d6 d7 d8 d11 d12 d13 d14 d15))

(deftest test-holiday-list-frankfurt
  (testing "Frankfurt Stock Exchange"
    (is (= frank-holiday-list frank-expected-list))
    (is (= "Frankfurt stock exchange" (frank-calendar :name)))))

;; Eurex 
(def eurex-calendar (new-calendar :germany :eurex))
(def eurex-holiday-list (holiday-list eurex-calendar (new-date 1 1 2003) (new-date 31 12 2004)))
(def eurex-expected-list (list d1 d2 d3 d4 d5 d6 d7 d8 d11 d12 d13 d14 d15))

(deftest test-holiday-list-eurex
  (testing "Eurex"
    (is (= eurex-holiday-list eurex-expected-list))
    (is (= "Eurex" (eurex-calendar :name)))))


;;; UK
  (def u1 (new-date  1  1 2004))
  (def u2 (new-date  9  4 2004))
  (def u3 (new-date 12  4 2004))
  (def u4 (new-date  3  5 2004))
  (def u5 (new-date 31  5 2004))
  (def u6 (new-date 30  8 2004))
  (def u7 (new-date 27 12 2004))
  (def u8 (new-date 28 12 2004))

  (def u11 (new-date  3  1 2005))
  (def u12 (new-date 25  3 2005))
  (def u13 (new-date 28  3 2005))
  (def u14 (new-date  2  5 2005))
  (def u15 (new-date 30  5 2005))
  (def u16 (new-date 29  8 2005))
  (def u17 (new-date 26 12 2005))
  (def u18 (new-date 27 12 2005))

  (def u21 (new-date  2  1 2006))
  (def u22 (new-date 14  4 2006))
  (def u23 (new-date 17  4 2006))
  (def u24 (new-date  1  5 2006))
  (def u25 (new-date 29  5 2006))
  (def u26 (new-date 28  8 2006))
  (def u27 (new-date 25 12 2006))
  (def u28 (new-date 26 12 2006))

  (def u31 (new-date  1  1 2007))
  (def u32 (new-date  6  4 2007))
  (def u33 (new-date  9  4 2007))
  (def u34 (new-date  7  5 2007))
  (def u35 (new-date 28  5 2007))
  (def u36 (new-date 27  8 2007))
  (def u37 (new-date 25 12 2007))
  (def u38 (new-date 26 12 2007))

;; UK Settlement 
(def uk-set-calendar (new-calendar :uk :settlement))
(def uk-set-holiday-list (holiday-list uk-set-calendar (new-date 1 1 2004) (new-date 31 12 2007)))
(def uk-set-expected-list (list u1 u2 u3 u4 u5 u6 u7 u8
                                u11 u12 u13 u14 u15 u16 u17 u18 
                                u21 u22 u23 u24 u25 u26 u27 u28
                                u31 u32 u33 u34 u35 u36 u37 u38))

(deftest test-holiday-list-uk-set
  (testing "UK Settlement"
    (is (= uk-set-holiday-list uk-set-expected-list))
    (is (= "UK settlement" (uk-set-calendar :name)))))

;; UK stock exchange 
(def uk-ex-calendar (new-calendar :uk :exchange))
(def uk-ex-holiday-list (holiday-list uk-ex-calendar (new-date 1 1 2004) (new-date 31 12 2007)))
(def uk-ex-expected-list (list u1 u2 u3 u4 u5 u6 u7 u8
                                u11 u12 u13 u14 u15 u16 u17 u18 
                                u21 u22 u23 u24 u25 u26 u27 u28
                                u31 u32 u33 u34 u35 u36 u37 u38))

(deftest test-holiday-list-uk-ex
  (testing "London stock exchange"
    (is (= (sort uk-ex-holiday-list) (sort uk-ex-expected-list)))
    (is (= "London stock exchange" (uk-ex-calendar :name)))))

;; UK Metals 
(def uk-met-calendar (new-calendar :uk :metals))
(def uk-met-holiday-list (holiday-list uk-met-calendar (new-date 1 1 2004) (new-date 31 12 2007)))
(def uk-met-expected-list (list u1 u2 u3 u4 u5 u6 u7 u8
                                u11 u12 u13 u14 u15 u16 u17 u18 
                                u21 u22 u23 u24 u25 u26 u27 u28
                                u31 u32 u33 u34 u35 u36 u37 u38))

(deftest test-holiday-list-uk-met
  (testing "London metals exchange"
    (is (= (sort uk-met-holiday-list) (sort uk-met-expected-list)))
    (is (= "London metals exchange" (uk-met-calendar :name)))))


;;; US

  (def us1  (new-date  1  1 2004))
  (def us2  (new-date 19  1 2004))
  (def us3  (new-date 16  2 2004))
  (def us4  (new-date  9  4 2004))
  (def us5  (new-date 31  5 2004))
  (def us6  (new-date 11  6 2004))
  (def us7  (new-date  5  7 2004))
  (def us8  (new-date  6  9 2004))
  (def us9  (new-date 11 10 2004))
  (def us10 (new-date 11 11 2004))
  (def us11 (new-date 25 11 2004))
  (def us12 (new-date 24 12 2004))
  
  (def us13 (new-date 31 12 2004))
  (def us14 (new-date 17  1 2005))
  (def us15 (new-date 21  2 2005))
  (def us16 (new-date 25  3 2005))
  (def us17 (new-date 30  5 2005))
  (def us18 (new-date  4  7 2005))
  (def us19 (new-date  5  9 2005))
  (def us20 (new-date 10 10 2005))
  (def us21 (new-date 11 11 2005))
  (def us22 (new-date 24 11 2005))
  (def us23 (new-date 26 12 2005))

  (def us24 (new-date  2  1 2006))
  (def us25 (new-date 16  1 2006))
  (def us26 (new-date 20  2 2006))
  (def us27 (new-date 14  4 2006))
  (def us28 (new-date 29  5 2006))
  (def us29 (new-date  4  7 2006))
  (def us30 (new-date  4  9 2006))
  (def us31 (new-date 23 11 2006))
  (def us32 (new-date 25 12 2006))

  (def us-hist1   (new-date 11  6 2004))
  (def us-hist2   (new-date 14  9 2001))
  (def us-hist3   (new-date 13  9 2001))
  (def us-hist4   (new-date 12  9 2001))
  (def us-hist5   (new-date 11  9 2001))
  (def us-hist6   (new-date 11  6 2004))
  (def us-hist7   (new-date 14  7 1977))
  (def us-hist8   (new-date 25  1 1973))
  (def us-hist9   (new-date 28 12 1972))
  (def us-hist10  (new-date 21  7 1969))
  (def us-hist11  (new-date 31  3 1969))
  (def us-hist12  (new-date 10  2 1969))
  (def us-hist13  (new-date  5  7 1968))
  ;paperwork crisis
  (def us-hist14  (new-date 12  6 1968))
  (def us-hist15  (new-date 19  6 1968))
  (def us-hist16  (new-date 26  6 1968))
  (def us-hist17  (new-date  3  7 1968))
  (def us-hist18  (new-date 10  7 1968))
  (def us-hist19  (new-date 17  7 1968))
  (def us-hist20  (new-date 20 11 1968))
  (def us-hist21  (new-date 27 11 1968))
  (def us-hist22  (new-date  4 12 1968))
  (def us-hist23  (new-date 11 12 1968))
  (def us-hist24  (new-date 18 12 1968))
  
  (def us-hist25  (new-date  4 11 1980))
  (def us-hist26  (new-date  2 11 1976))
  (def us-hist27  (new-date  7 11 1972))
  (def us-hist28  (new-date  5 11 1968))
  (def us-hist29  (new-date  3 11 1964))


;; US Settlement 
(def us-set-calendar (new-calendar :us :settlement))
(def us-set-holiday-list (holiday-list us-set-calendar (new-date 1 1 2004) (new-date 31 12 2005)))
(def us-set-expected-list (list us1 us2 us3 us5 us7 us8 us9 us10 us11 us12
                                us13 us14 us15 us17 us18 us19 us20 us21 us22 us23))

(deftest test-holiday-list-us-set
  (testing "US settlement"
    (is (= us-set-holiday-list us-set-expected-list))
    (is (= "US settlement" (us-set-calendar :name)))))

;; US Gouvernment Bond Market
(def us-bond-calendar (new-calendar :us :government-bond))
(def us-bond-holiday-list (holiday-list us-bond-calendar (new-date 1 1 2004) (new-date 31 12 2004)))
(def us-bond-expected-list (list us1 us2 us3 us4 us5 us7 us8 us9 us10 us11 us12))

(deftest test-holiday-list-us-bond
  (testing "US government bond market"
    (is (= us-bond-holiday-list us-bond-expected-list))
    (is (= "US government bond market" (us-bond-calendar :name)))))

;; New York stock exchage
(def us-nyse-calendar (new-calendar :us :nyse))
(def us-nyse-holiday-list (holiday-list us-nyse-calendar (new-date 1 1 2004) (new-date 31 12 2006)))
(def us-nyse-expected-list (list us1 us2 us3 us4 us5 us6 us7 us8 us11 us12
                                us14 us15 us16 us17 us18 us19 us22 us23
                                us24 us25 us26 us27 us28 us29 us30 us31 us32))

(deftest test-holiday-list-us-nyse
  (testing "New York stock exchange"
    (is (= us-nyse-holiday-list us-nyse-expected-list))
    (is (= "New York stock exchange" (us-nyse-calendar :name)))))

;; US historical holidays

(defn- test-set-holidays [date-set]
  (map #(business-day? % us-nyse-calendar) date-set))

(def us-hist-holidays [us-hist1  us-hist2  us-hist3  us-hist4  us-hist5  us-hist6
                       us-hist7  us-hist8  us-hist9  us-hist10 us-hist11 us-hist12
                       us-hist13 us-hist14 us-hist15 us-hist16 us-hist17 us-hist18
                       us-hist19 us-hist20 us-hist21 us-hist22 us-hist23 us-hist24
                       us-hist25 us-hist26 us-hist27 us-hist28 us-hist29])

(deftest test-hist-holidays-us
  (is (zero? (count (filter true? (test-set-holidays us-hist-holidays)))))) 
