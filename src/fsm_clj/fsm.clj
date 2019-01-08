(ns fsm-clj.fsm
  (:require
   [clojure.spec.alpha :as s]))

;;;;;;;;;;;;;;;;;;;;;;
;;; Base namespace ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Creators
;;;

(defn- all
  [lst]
  (if (empty? lst)
    true
    (reduce #(and %2 %1)
            true
            lst)))

(defn- all-contains
  [data lst]
  (all
   (map #(contains? data %)
        lst)))

;; FIX exception
(defn valid-template
  [template]
  (and (all (map (fn [[_ v]] (and
                              (contains? v :edges)
                              (all (map #(and (contains? % :state)
                                              (contains? % :valid?))
                                        (:edges v)))
                              (contains? v :type)))
                 template))
       (let [states (set (keys template))
             in-states (set (reduce concat
                                    (map (fn [[_ v]]
                                           (map #(:state %)
                                                (:edges v)))
                                         template)))]
         (all-contains states in-states))))

(defn valid-fsm
  [{:keys [template state data states] :as fsm}]
  (and
   (keyword? state)
   (= states (keys template))
   (valid-template template)))

(defn make-fsm
  [template start-state fsm-data]
  (let [fsm {:template template
             :state start-state
             :data fsm-data
             :states (keys template)}]
    (assert (valid-fsm fsm))
    fsm))

;;;
;;; Geters
;;;

(defn get-data
  [fsm]
  (:data fsm))

(defn get-state
  [fsm]
  (:state fsm))

(defn get-template
  [fsm]
  (:template fsm))

(defn get-edges
  [template state]
  (:edges (get template state)))

;;;
;;; Updaters
;;;

(defn set-state
  [fsm state]
  (assoc fsm :state state))

(defn update-data
  [fsm updater]
  (update fsm :data updater))


;;;
;;; Update
;;;

(defn- get-next-states
  [edges data]
  (filter #((:valid? %1) data) edges))

(defn update-fsm
  [fsm updater & {:keys [choice-fn]
                  :or {choice-fn first}}]
  (let [nfsm (update-data fsm updater)
        state (get-state nfsm)
        fsm-data (get-data nfsm)
        template (get-template nfsm)
        current-data (get-edges template state)
        next-states (get-next-states current-data fsm-data)]
    (println next-states)
    (if (empty? next-states)
      nfsm
      (set-state nfsm
                 (:state
                  (choice-fn next-states))))))
