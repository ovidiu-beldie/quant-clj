
(defproject quant "1.0.0-SNAPSHOT"
  :description "quant-clj, an implementation of quantLib in Clojure"
  :dependencies [[org.clojure/clojure "1.4.0"]
				 [org.clojure/algo.generic "0.1.0"]
				 [incanter "1.4.1"]
				 [colt "1.2.0"]]
  :test-selectors {:current :current
				   :all (constantly true)})
