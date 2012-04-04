; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.time.daycounter)

(defprotocol Daycounter
  (get-name [this])
  (day-count [this date1 date2])
  (year-fraction [this date1 date2 refPeriodStart refPeriodEnd]))

