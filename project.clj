(defproject fsm-clj "0.0.1"
  :description "Finite-state machine framework on Clojure"
  :url "https://github.com/DarkNoys/fsm-clj"
  :plugins [[lein-codox "0.10.6"]]
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :codox {:metadata
          {:doc/format :markdown}}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [prismatic/schema "1.1.10"]
                 [cheshire "5.8.1"]])
