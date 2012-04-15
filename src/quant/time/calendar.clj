; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendar
  (:use [quant.time.date :only (new-date day month year next-day)]
        [quant.time.calendars.calendar-impl :only (new-calendar-impl, weekend-day-impl?)]
        [quant.time.calendars.germany]
        [quant.time.calendars.uk]
        [quant.time.calendars.us]
  )
  (:import [java.util GregorianCalendar Calendar]))

(defn new-calendar [country market]
  (new-calendar-impl country market))

(defn business-day? [date {:keys [fn-business-day? fn-weekend-day? added-holidays removed-holidays]}]
  "Checks if a date is a business day
   (not holiday or weekend)"
  (if (fn-weekend-day? date)
    false
    (if (added-holidays date)
      false
      (if (removed-holidays date)
        true
        (fn-business-day? date)))))

(defn weekend-day?
  "Checks if a date is a weekend day"
  ([date]
    (weekend-day-impl? date))
  ([date calendar]
    (weekend-day-impl? date calendar)))

(defn holiday-list 
  "Returns the list of holidays between 2 dates
   (inclusive) for a given calendar"
  ([calendar start-date end-date]
    (holiday-list calendar start-date end-date false))
  ([calendar start-date end-date include-weekends]
    (loop [date start-date, holidays []]
      (if (= date (next-day end-date))
        holidays
        (let [holidays-upd
                (if (not (business-day? date calendar))
                  (if (not (weekend-day? date calendar))
                    (conj holidays date)
                    (if include-weekends
                      (conj holidays date)
                      holidays))
                  holidays)]
          (recur (next-day date) holidays-upd))))))

