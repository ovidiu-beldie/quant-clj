; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.date
  (:import [java.util GregorianCalendar Calendar]))

(defprotocol Date
  (day [this])
  (month [this])
  (year [this]))

(extend-protocol Date
  java.util.Calendar
  
  (day [this]
    (.get this Calendar/DAY_OF_MONTH))

  (month [this]
    (inc (.get this Calendar/MONTH)))

  (year [this]
    (.get this Calendar/YEAR)))

(defn new-date [day month year]
  (GregorianCalendar. year (dec month) day))

(defn leap-year [date]
  (zero? (rem (year date) 4)))

(defn end-of-month? [date]
  (let [month-length {1 31, 3 31, 4 30, 5 31, 6 30,
          7 31, 8 31, 9 30, 10 31, 11 30, 12 31}
        m (month date)
        d (day date)]
    (if (= m 2)
        (if (leap-year date)
          (= d 29)
          (= d 28))
        (= d (month-length m)))))
