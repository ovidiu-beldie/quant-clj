; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendar
  (:use [quant.time.date :only (new-date day month year next-day)])
  (:import [java.util GregorianCalendar Calendar]))

(defstruct calendar :name :fn-business-day? :fn-weekend-day? :added-holidays :removed-holidays)

(defn calendar-selector [country market]
  (list country market))

(defmulti new-calendar calendar-selector)

(defmethod new-calendar :default [country market]
  (throw (IllegalArgumentException. "Unknown country or market")))

(defn business-day? [{:keys [fn-business-day? fn-weekend-day? added-holidays removed-holidays]} date]
  (if (fn-weekend-day? date)
    false
    (if (added-holidays date)
      false
      (if (removed-holidays date)
        true
        (fn-business-day? date)))))

(defn weekend-day? [{:keys [fn-weekend-day?]} date]
  (fn-weekend-day? date))

(defn holiday-list 
  ([calendar start-date end-date]
    (holiday-list calendar start-date end-date false))
  ([calendar start-date end-date include-weekends]
    (loop [date start-date, holidays []]
      (do
      (prn "*********************")
      (prn "holiday-list: day=" (day date))
      (prn "*********************")
      (prn "holiday-list: holidays loop start=" (map day holidays))
      (if (= date (next-day end-date))
        (do
        (prn "holiday-list : holidays return=" (map day holidays))
        holidays)
        (let [holidays-upd
                (if (not (business-day? calendar date))
                  (if (not (weekend-day? calendar date))
                    (conj holidays date)
                    (if include-weekends
                      (conj holidays date)
                      holidays))
                  holidays)
              _ (prn "*********************")
              _ (prn "holiday-list: holidays-upd=" (map day holidays-upd))
              ]
          (recur (next-day date) holidays-upd)))))))

