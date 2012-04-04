; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.daycounters.thirty360
  (:use
    [quant.time.date :only (day month year new-date Date)]
    [quant.time.daycounter]))

(declare adjust-dates)

(def conv-map {:usa :us, :bond-basis :us, :european :eu, :eurobond-basis :eu, :italian :it})

(def name-us "30/360 (Bond Basis)")
(def name-eu "30E/360 (Eurobond Basis)")
(def name-it "30/360 30/360 (Italian)")

(def name-map {:usa name-us, :bond-basis name-us, :european name-eu, :eurobond-basis name-eu, :italian name-it})

(defrecord Thirty360 [the-name convention])

(extend-protocol Daycounter
  Thirty360

  (get-name [this]
    (:the-name this))

  (day-count [this date1 date2]
    (let [[d1 d2] (adjust-dates date1 date2 (:convention this))
          y (* 360 (- (year d2) (year d1)))
          m (* 30 (- (month d2) (month d1) 1))
          day1 (max 0 (- 30 (day d1)))
          day2 (min 30 (day d2))]
      (+ y m day1 day2)))

  (year-fraction [this d1 d2 _ _]
    (/ (day-count this d1 d2) 360)))


(defn new-thirty360
  ([]
    (new-thirty360 :bond-basis))
  ([convention]
    (let [the-name (name-map convention)
          conv (conv-map convention)]
      (if (nil? conv)
        (throw (IllegalArgumentException. "Unknown 30/360 convention"))
        (Thirty360. the-name conv)))))

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
