(set-env!
 :source-paths #{"src"}
 :dependencies '[[clj-time "0.14.4"]
                 [org.clojure/core.async "0.4.474"]])


;;
;; CIDER
;;
(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.18.0"]
                [refactor-nrepl "2.4.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware*
       concat '[cider.nrepl/cider-middleware
                refactor-nrepl.middleware/wrap-refactor])



(task-options!
 pom {:project 'fsm-clj
      :version "0.1.0"}
 ;; jar {:main 'fsm-clj.core}
 ;; aot {:all true}
 repl {:init-ns 'fsm-clj.type
       :eval '(set! *print-length* 20)})

;;
;; Uberjar
;;
;;(deftask build []
;;  (comp
;;   (aot)
;;   (pom)
;;   (uber)
;;   (jar))

