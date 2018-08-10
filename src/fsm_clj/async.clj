(ns fsm-clj.async
  (:require
   [clojure.core.async :as a]
   [fsm-clj.fsm :as fsm]))

;;;;;;;;;;;;;;;;;
;;; Async FSM ;;;
;;;;;;;;;;;;;;;;;

;;;
;;; Async controller
;;;

;; Maker

(defn make-async-controller
  [controller state-agent]
  {:controller controller
   :state-agent state-agent})

;; Geters

(defn acont-get-controller
  [acont]
  (:controller acont))

(defn acont-get-state
  [acont]
  (:state-agent acont))

;;;
;;; Async action
;;;
(defn- apply-command-fun
  [state fun]
  (let [data (fsm/state-get-data state)]
    (fsm/state-set-data state
                        (fun data))))

(defn- command-filter?
  [val]
  (or
   (keyword? val)
   (fn? val)))

(defn- control-fsm-fun
  "return false if need terminate go-loop"
  [command state-agent]
  (when (command-filter? command)
    (cond
      (fn? command) (try
                      (do (send
                           state-agent
                           apply-command-fun command)
                          (send
                           state-agent
                           fsm/apply-fsm)
                          true)
                      (catch Exception e (str "Incorrect command, fsm or state: " (.getMessage e))
                             false))
      (keyword? command) (condp = command
                           :stop false
                           true)
      :else true)))

(defn start-fsm
  "start async proc state by chengers."
  [init-state]
  (let [controller (a/chan)
        state-agent (agent init-state)]
    (a/go-loop []
      (let [command (a/<!! controller)
            continue? (control-fsm-fun
                       command
                       state-agent)]
        (if continue?
          (recur)
          nil)))
    (make-async-controller
     controller
     state-agent)))

(defn stop-fsm
  "stop acont fsm"
  [acont]
  (a/>!! (acont-get-controller acont)
         :stop))

;;;
;;; Control action
;;;

(defn acont-send-command
  [acont fun]
  {:pre [(fn? fun)]}
  (a/>!! (acont-get-controller acont)
         fun))
