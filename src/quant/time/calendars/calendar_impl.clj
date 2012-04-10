; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendars.calendar-impl
  (:use [quant.time.date :only (new-date day month year next-day)]
        [quant.time.calendar :only (calendar)])
  (:import [java.util GregorianCalendar Calendar]))

(declare catholic-easter-mondays, orthodox-easter-mondays)

(def calendar-map {:catholic catholic-easter-mondays, :orthodox orthodox-easter-mondays})

(defn easter-monday [year calendar-type]
  ((calendar-map calendar-type) (- year 1901)))

;;; Common Holidays

(defn new-years-day       [_  year] (new-date 1 1 year))
(defn good-friday         [em year] (new-date (- em 3) year))
(defn easter-monday       [em year] (new-date em year))
(defn ascension-thursday  [em year] (new-date (+ em 38) year))
(defn whit-monday         [em year] (new-date (+ em 49) year))
(defn corpus-christi      [em year] (new-date (+ em 59) year))
(defn labour-day          [_  year] (new-date 1 5 year))
(defn christmas-eve       [_  year] (new-date 24 12 year))
(defn christmas           [_  year] (new-date 25 12 year))
(defn boxing-day          [_  year] (new-date 26 12 year))
(defn new-years-eve       [_  year] (new-date 31 12 year))

(defn default-weekend-fn [date]
  (let [day-of-week (.get date Calendar/DAY_OF_WEEK)
        weekend-days #{Calendar/SATURDAY Calendar/SUNDAY}
        weekend-day (weekend-days day-of-week)]
    (not (nil? weekend-day))))

(defn compare-day-and-month [d1 d2]
  "Compares two dates without considering the values of the years"
  (do
  (prn "day d1=" (day d1) "day d2=" (day d2))
  (and (= (day d1) (day d2)) (= (month d1) (month d2)))))

(defn date-in-set? [set-of-dates date compare-fn]
  "Searches a date in a set of dates using the provided
   comparaison function to test for equality"
  (do
  ;(prn "set-of-dates=" (map #(day %) set-of-dates))
  (some true? (map #(compare-fn date %) set-of-dates))))

(defn make-christian-business-day-fn [holiday-fns easter-table]
  (fn [date]
    (let [easter-monday (easter-table (- (year date) 1901))
          ;_ (prn "make-christian-business-day-fn: holiday-fns=" holiday-fns)
          holidays (map #(% easter-monday (year date)) holiday-fns)
          ;_ (prn "make-christian-business-day-fn: holidays=" holidays)
          ]
      (not (date-in-set? holidays date compare-day-and-month)))))

(defn make-western-business-day-fn [holiday-fns]
  (make-christian-business-day-fn holiday-fns catholic-easter-mondays))

(defn make-orthodox-business-day-fn [holiday-fns]
  (make-christian-business-day-fn holiday-fns orthodox-easter-mondays))

(def calendar-business-day-fn-map {:western make-western-business-day-fn})

(defn make-calendar 
  ([the-name holiday-fns calendar-type]
    (make-calendar the-name holiday-fns calendar-type default-weekend-fn))
  ([the-name holiday-fns calendar-type weekend-fn]
    (let [business-day-fn (calendar-business-day-fn-map calendar-type)]
      (struct calendar the-name (business-day-fn holiday-fns)  weekend-fn (ref #{}) (ref #{})))))

;;; Easter Monday tables

(def catholic-easter-mondays
       [98,  90, 103,  95, 114, 106,  91, 111, 102,   ; 1901-1909
   87, 107,  99,  83, 103,  95, 115,  99,  91, 111,   ; 1910-1919
   96,  87, 107,  92, 112, 103,  95, 108, 100,  91,   ; 1920-1929
  111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   ; 1930-1939
   85, 104,  96, 116, 101,  92, 112,  97,  89, 108,   ; 1940-1949
  100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   ; 1950-1959
  109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   ; 1960-1969
   89, 102,  94, 113, 105,  90, 110, 101,  86, 106,   ; 1970-1979
   98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   ; 1980-1989
  106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   ; 1990-1999
  115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   ; 2000-2009
   95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   ; 2010-2019
  104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   ; 2020-2029
  112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   ; 2030-2039
   93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   ; 2040-2049
  101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   ; 2050-2059
  110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   ; 2060-2069
   90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   ; 2070-2079
   99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   ; 2080-2089
  107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   ; 2090-2099
   88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   ; 2100-2109
   97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   ; 2110-2119
  106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   ; 2120-2129
   86, 106,  98, 110, 102,  94, 114,  98,  90, 110,   ; 2130-2139
   95,  86, 106,  91, 111, 102,  94, 107,  99,  90,   ; 2140-2149
  103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   ; 2150-2159
   84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   ; 2160-2169
   92, 112, 104,  95, 108, 100,  92, 111,  96,  88,   ; 2170-2179
  108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   ; 2180-2189
  116, 101,  93, 112,  97,  89, 109, 100,  85, 105])  ; 2190-2199

(def orthodox-easter-mondays
      [105, 118, 110, 102, 121, 106, 126, 118, 102,   ; 1901-1909
  122, 114,  99, 118, 110,  95, 115, 106, 126, 111,   ; 1910-1919
  103, 122, 107,  99, 119, 110, 123, 115, 107, 126,   ; 1920-1929
  111, 103, 123, 107,  99, 119, 104, 123, 115, 100,   ; 1930-1939
  120, 111,  96, 116, 108, 127, 112, 104, 124, 115,   ; 1940-1949
  100, 120, 112,  96, 116, 108, 128, 112, 104, 124,   ; 1950-1959
  109, 100, 120, 105, 125, 116, 101, 121, 113, 104,   ; 1960-1969
  117, 109, 101, 120, 105, 125, 117, 101, 121, 113,   ; 1970-1979
   98, 117, 109, 129, 114, 105, 125, 110, 102, 121,   ; 1980-1989
  106,  98, 118, 109, 122, 114, 106, 118, 110, 102,   ; 1990-1999
  122, 106, 126, 118, 103, 122, 114,  99, 119, 110,   ; 2000-2009
   95, 115, 107, 126, 111, 103, 123, 107,  99, 119,   ; 2010-2019
  111, 123, 115, 107, 127, 111, 103, 123, 108,  99,   ; 2020-2029
  119, 104, 124, 115, 100, 120, 112,  96, 116, 108,   ; 2030-2039
  128, 112, 104, 124, 116, 100, 120, 112,  97, 116,   ; 2040-2049
  108, 128, 113, 104, 124, 109, 101, 120, 105, 125,   ; 2050-2059
  117, 101, 121, 113, 105, 117, 109, 101, 121, 105,   ; 2060-2069
  125, 110, 102, 121, 113,  98, 118, 109, 129, 114,   ; 2070-2079
  106, 125, 110, 102, 122, 106,  98, 118, 110, 122,   ; 2080-2089
  114,  99, 119, 110, 102, 115, 107, 126, 118, 103,   ; 2090-2099
  123, 115, 100, 120, 112,  96, 116, 108, 128, 112,   ; 2100-2109
  104, 124, 109, 100, 120, 105, 125, 116, 108, 121,   ; 2110-2119
  113, 104, 124, 109, 101, 120, 105, 125, 117, 101,   ; 2120-2129
  121, 113,  98, 117, 109, 129, 114, 105, 125, 110,   ; 2130-2139
  102, 121, 113,  98, 118, 109, 129, 114, 106, 125,   ; 2140-2149
  110, 102, 122, 106, 126, 118, 103, 122, 114,  99,   ; 2150-2159
  119, 110, 102, 115, 107, 126, 111, 103, 123, 114,   ; 2160-2169
   99, 119, 111, 130, 115, 107, 127, 111, 103, 123,   ; 2170-2179
  108,  99, 119, 104, 124, 115, 100, 120, 112, 103,   ; 2180-2189
  116, 108, 128, 119, 104, 124, 116, 100, 120, 112])  ; 2190-2199
