; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.calendars.europe
  (:use [quant.time.calendar :only (new-calendar)]
        [quant.time.calendars.calendar-impl :only (make-calendar)]
        [quant.time.date :only (new-date)])
  (:require [quant.time.calendars.calendar-impl :as c]))

;; Germany

(def ger-holidays-common #{c/new-years-day, c/good-friday, c/easter-monday,
     c/labour-day, c/christmas-eve, c/christmas, c/boxing-day, c/new-years-eve})

(defmethod new-calendar '(:germany :settlement) [country market]
  (let [the-name "German Settlement"
        holiday-fns (conj ger-holidays-common c/ascension-thursday, 
                    c/whit-monday, c/corpus-christi, #(new-date 3 10))]
    (make-calendar the-name holiday-fns :western)))

(defmethod new-calendar '(:germany :frankfurt-stock-exchange) [country market]
  (let [the-name "Frankfurt stock exchange"
        holiday-fns ger-holidays-common]
    (make-calendar the-name holiday-fns :western)))

(defmethod new-calendar '(:germany :xetra) [country market]
  (let [the-name "Xetra"
        holiday-fns ger-holidays-common]
    (make-calendar the-name holiday-fns :western)))

(defmethod new-calendar '(:germany :eurex) [country market]
  (let [the-name "Eurex"
        holiday-fns ger-holidays-common]
    (make-calendar the-name holiday-fns :western)))

(defmethod new-calendar '(:germany :euwax) [country market]
  (let [the-name "Euwax"
        holiday-fns (conj ger-holidays-common c/whit-monday)]
    (make-calendar the-name holiday-fns :western)))
