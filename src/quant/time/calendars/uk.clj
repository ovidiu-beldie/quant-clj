; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendars.uk
  (:use [quant.time.date :only (new-date next-day day)])
  (:require [quant.time.calendars.calendar-impl :as c]))

(defn- new-years-day [_ year]
  (let [d1 (new-date 1 1 year)
        d2 (next-day d1)
        d3 (next-day d2)
        week-days (filter #(not (c/weekend-day-impl? %)) (list d1 d2 d3))]
    [(first week-days)]))

;first-monday-may
(defn- early-may-bank-holiday [_ year]
  (loop [date (new-date 1 5 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

;last-monday-may
(defn- spring-bank-holiday [_ year]
  (if (= year 2002)
    [nil]
    (loop [date (new-date 25 5 year)]
      (if (c/monday? date)
        [date]
        (recur (next-day date))))))
;last-monday-august
(defn- summer-bank-holiday [_ year]
  (loop [date (new-date 25 8 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

(defn- christmas [_ year]
  (let [xmas (new-date 25 12 year)]
    (if (not (c/weekend-day-impl? xmas))
      [xmas]
      [(new-date 27 12 year)]))) 


(defn- boxing-day [_ year]
  (let [date (new-date 26 12 year)]
    (if (not (c/weekend-day-impl? date))
      [date]
      [(new-date 28 12 year)])))


(defn- golden-jubilee [_ year]
  (if (= year 2002) [(new-date 3 6 year)] [nil]))

(defn- special-spring-holiday [_ year]
  (if (= year 2002) [(new-date 4 6 year)] [nil]))

(defn- special-new-years-eve [_ year]
  (if (= year 1999) [(new-date 31 12 year)] [nil]))

(def holiday-fns #{new-years-day, c/good-friday, c/easter-monday, 
    early-may-bank-holiday, spring-bank-holiday, summer-bank-holiday, christmas, 
    boxing-day, golden-jubilee, special-spring-holiday, 
    special-new-years-eve})

(defmethod c/new-calendar-impl '(:uk :settlement) [country market]
  (c/make-calendar "UK settlement" holiday-fns :western))

(defmethod c/new-calendar-impl '(:uk :exchange) [country market]
  (c/make-calendar "London stock exchange" holiday-fns :western))

(defmethod c/new-calendar-impl '(:uk :metals) [country market]
  (c/make-calendar "London metals exchange" holiday-fns :western))
