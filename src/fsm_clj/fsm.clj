(ns fsm-clj.fsm
  (:require
   [clojure.spec.alpha :as s])
  (:use
   [fsm-clj.type]))


;;;;;;;;;;;;;;;;;;;;;;
;;; Base namespace ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Creators
;;;


(defn make-fsm
  ([& {:keys [states rules invariant]
       :or {states #{}
            rules {}
            invariant {}}}]
   (let [fsm {:states states
              :rules rules
              :invariant invariant}]
       (assert (s/valid? :type.fsm/base fsm)
               (s/explain :type.fsm/base fsm))
       fsm)))

(defn make-state
  [fsm init-state data]
  {:current init-state
   :fsm fsm
   :data data})

;;;
;;; Geters
;;;

;;; fsm

(defn fsm-get-curent
  [fsm]
  (:current fsm))

(defn fsm-get-states
  [fsm]
  (:states fsm))

(defn fsm-get-rules
  [fsm]
  (:rules fsm))

(defn fsm-get-invariant
  [fsm]
  (:invariant fsm))

;;; state

(defn state-get-fsm
  [state]
  (:fsm state))

(defn state-get-cur-state
  [state]
  (:current state))

(defn state-get-data
  [state]
  (:data state))

;;;
;;; Seters
;;;
(defn state-set-cur-state
  [state new-state]
  (assoc state :current new-state))

(defn state-set-data
  [state new-state]
  (assoc state :data new-state))

;;;
;;; Actions
;;;
(defn- filter-rule
  [rule state]
  (let [apply-key (fn [{:keys [key] :as val} state]
                    (key state))]
    (filter #(let [[key val] %]
               (apply-key val state))
            rule)))
(defn apply-fsm
  [state]
  (let [fsm (state-get-fsm state)
        state-data (state-get-data state)
        current (state-get-cur-state state)
        rules (fsm-get-rules fsm)
        cur-rule (get rules current)
        [nkey nval] (first
                     (filter-rule cur-rule
                                  state-data))]
    (if (nil? nkey)
      state
      (-> state
          (state-set-cur-state nkey)
          (state-set-data
           ((:action nval) state-data))))))


