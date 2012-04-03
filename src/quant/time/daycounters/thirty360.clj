; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.daycounters.thirty360
  (:use [quant.time.date :only (day month year new-date Date)]))

(def conv-map {:usa :us, :bond-basis :us, :european :eu, :eurobond-basis :eu, :italian :it})

(defn adjust-dates [d1 d2 conv]
  (let [us  (fn []
              (if (and (= (day d2) 31) (< (day d1) 30))
                [d1 (new-date 1 (inc (month d2)) (year d2))]
                [d1 d2]))
        eu  (fn [] [d1 d2])
        it-1  (fn [d]
                (if (and (= (month d) 2) (> (day d) 27))
                  (reify Date
                    (day [this] 30)
                    (month [this] 2)
                    (year [this] (year d)))
                  d))
        it  (fn [] [(it-1 d1) (it-1 d2)])
        conv-map {:us us, :eu eu, :it it}
        func (conv-map conv)]
    ((conv-map conv))))

(defn day-count [date1 date2 convention]
  (let [[d1 d2] (adjust-dates date1 date2 (conv-map convention))
        y (* 360 (- (year d2) (year d1)))
        m (* 30 (- (month d2) (month d1) 1))
        day1 (max 0 (- 30 (day d1)))
        day2 (min 30 (day d2))]
    (+ y m day1 day2)))

        
    
