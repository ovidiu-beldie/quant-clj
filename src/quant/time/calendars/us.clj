; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendars.us
  (:use [quant.time.date :only (new-date, next-day, day, month)])
  (:require [quant.time.calendars.calendar-impl :as c]
            [quant.time.date :as d]))

(defn- new-years-day-settlement [_ year]
  (let [first-weekday (fn [list-of-days] (first (filter #(not (c/weekend-day-impl? %)) list-of-days)))
        d1 (new-date 1 1 year)
        d2 (next-day d1)
        d1n (new-date 1 1 (inc year))
        d2n (next-day d1n)
        new-years-next-year (if (nil? (first-weekday (list d1n d2n))) 
                              (new-date 31 12 year)
                              nil)]
    [(first-weekday (list d1 d2)) new-years-next-year]))

(defn- new-years-day [_ year]
  (let [date (new-date 1 1 year)]
    (if (c/sunday? date)
      [(next-day date)]
      [date])))

;third-monday-jan
(defn- martin-luther-king-bday [_ year]
  (loop [date (new-date 15 1 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

(defn- martin-luther-king-bday-after-98 [_ year]
  (if (< year 1998)
    nil
    (loop [date (new-date 15 1 year)]
      (if (c/monday? date)
        [date]
        (recur (next-day date))))))

;third-monday-feb
(defn- washington-bday [_ year]
  (loop [date (new-date 15 2 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

;last-monday-may
(defn- memorial-day [_ year]
  (loop [date (new-date 25 5 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

(defn- independence-day [_ year]
  (let [d1 (new-date 3 7 year)
        d2 (next-day d1)
        d3 (next-day d2)
        week-days (filter #(not (c/weekend-day-impl? %)) (list d2 d3 d1))]
    [(first week-days)]))

;first-monday-sept
(defn- labor-day [_ year]
  (loop [date (new-date 1 9 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

;second-monday-october
(defn- columbus-day [_ year]
  (loop [date (new-date 8 10 year)]
    (if (c/monday? date)
      [date]
      (recur (next-day date)))))

(defn- veterans-day [_ year]
  (let [d1 (new-date 10 11 year)
        d2 (next-day d1)
        d3 (next-day d2)
        week-days (filter #(not (c/weekend-day-impl? %)) (list d2 d3 d1))]
    [(first week-days)]))

;fourth-thursday-november
(defn- thanksgiving [_ year]
  (loop [date (new-date 22 11 year)]
    (if (c/thursday? date)
      [date]
      (recur (next-day date)))))

(defn- christmas [_ year]
  (let [d1 (new-date 24 12 year)
        d2 (next-day d1)
        d3 (next-day d2)
        week-days (filter #(not (c/weekend-day-impl? %)) (list d2 d3 d1))]
    [(first week-days)]))

(defn- reagan-funeral [_ year]
  (if (not (= year 2004))
    [nil]
    [(new-date 11 6 year)]))

(defn- september-11 [_ year]
  (if (not (= year 2001))
    [nil]
    (let [d1 (new-date 11 9 year)
          dates (take 4 (iterate next-day d1))]
      (vec dates))))

(defn- ford-funeral [_ year]
  (if (not (= year 2007))
    [nil]
    [(new-date 2 1 year)]))

(defn- nixon-funeral [_ year]
  (if (not (= year 1994))
    [nil]
    [(new-date 27 4 year)]))

(defn- president-election-day [_ year]
  (if (or (> year 1980) (not (zero? (rem year 4))))
    [nil]
    (loop [date (new-date 1 11 year)]
      (if (c/tuesday? date)
        [date]
        (recur (next-day date))))))

(defn- blackout-1977 [_ year]
  (if (not (= year 1977))
    [nil]
    [(new-date 14 7 year)]))
    
(defn- johnson-funeral [_ year]
  (if (not (= year 1973))
    [nil]
    [(new-date 25 1 year)]))

(defn- truman-funeral [_ year]
  (if (not (= year 1972))
    [nil]
    [(new-date 28 12 year)]))

(defn- lunar-exploration [_ year]
  (if (not (= year 1969))
    [nil]
    [(new-date 21 7 year)]))

(defn- eisenhower-funeral [_ year]
  (if (not (= year 1969))
    [nil]
    [(new-date 31 3 year)]))

(defn- heavy-snow [_ year]
  (if (not (= year 1969))
    [nil]
    [(new-date 10 2 year)]))

(defn- day-after-independence-day [_ year]
  (if (not (= year 1968))
    [nil]
    [(new-date 5 7 year)]))

(defn- paperwork-crisis [_ year]
  (if (not (= year 1968))
    [nil]
    (let [date (new-date 12 6 1968)
          datess (iterate next-day date)
          _ (prn "a few dates=" (take 10 (map d/year datess)))
          dates (take-while #(= (d/year %) 1968) (iterate next-day date))]
      (vec (filter c/wednesday? dates)))))


(def holidays-common #{memorial-day, independence-day,
    labor-day, thanksgiving, christmas})

(defmethod c/new-calendar-impl '(:us :settlement) [country market]
  (let [the-name "US settlement"
        holiday-fns (conj holidays-common new-years-day-settlement, 
            martin-luther-king-bday, washington-bday, memorial-day, 
            columbus-day, veterans-day)]
    (c/make-calendar the-name holiday-fns :western)))

(defmethod c/new-calendar-impl '(:us :nyse) [country market]
  (let [the-name "New York stock exchange"
        holiday-fns (conj holidays-common new-years-day, 
          washington-bday, c/good-friday, memorial-day, 
          reagan-funeral, martin-luther-king-bday-after-98, 
          september-11,ford-funeral, president-election-day,
          blackout-1977, johnson-funeral, truman-funeral,
          lunar-exploration, eisenhower-funeral, 
          heavy-snow, day-after-independence-day, 
          paperwork-crisis, nixon-funeral)]
    (c/make-calendar the-name holiday-fns :western)))

(defmethod c/new-calendar-impl '(:us :government-bond) [country market]
  (let [the-name "US government bond market"
        holiday-fns (conj holidays-common new-years-day, 
          martin-luther-king-bday, washington-bday, c/good-friday, 
          memorial-day, columbus-day, veterans-day)]
    (c/make-calendar the-name holiday-fns :western)))

(defmethod c/new-calendar-impl '(:us :nerc) [country market]
  (let [the-name "North American Energy Reliability Council"
        holiday-fns (conj holidays-common new-years-day)]
    (c/make-calendar the-name holiday-fns :western)))
