(defproject fsm-clj "0.0.1"
  :description "Finite-state machine framework on Clojure"
  :url "https://github.com/DarkNoys/fsm-clj"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :profiles {
             :1.10 {:dependencies [[org.clojure/clojure "1.10.0"]]}
             :test {:dependencies [[midje "1.9.6"]]
                    :plugins [[lein-midje "3.2.1"]]}
             :dev [:1.10 :test
                   {:plugins [[lein-codox "0.10.6"]]}]}
  :source-paths ["src"]
  :test-paths ["test"]
  :codox {:metadata
          {:doc/format :markdown}}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [prismatic/schema "1.1.10"]
                 [cheshire "5.8.1"]])
