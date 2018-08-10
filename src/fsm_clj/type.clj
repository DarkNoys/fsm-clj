(ns fsm-clj.type
  (:require [clojure.spec.alpha :as s]))

;;;;;;;;;;;;;;;;;;
;;; Spec types ;;;
;;;;;;;;;;;;;;;;:;

;;;
;;; Help type
;;;
(s/def :type.fsm.field/states
  set?)

(s/def :type.fsm.field.rules/action
  fn?)
(s/def :type.fsm.field.rules/key
  fn?)
(s/def :type.fsm.field/rules
  (s/and map?
         (s/map-of keyword?
                   (s/map-of keyword?
                             (s/keys
                              :req-un [:type.fsm.field.rules/action
                                       :type.fsm.field.rules/key])))))

(s/def :type.fsm.field/invariant
  (s/map-of keyword? fn?))

;;;
;;; Base finite-state machine type
;;;
(defn all-contains?
  "return true if all in v contains in s"
  [s v]
  (every? #(contains? s %)
          v))

(s/def :type.fsm/base
  (s/and (s/keys
          :req-un [:type.fsm.field/states
                   :type.fsm.field/invariant
                   :type.fsm.field/rules])
         ;; Valid :rules
         (fn [{:keys [states rules] :as fsm}]
           (let [prepare-rules (map #(let [[key vals] %]
                                       [key (keys vals)])
                                    rules)]
             (every? (fn [[key val-keys]]
                       (and (contains? states key)
                            (all-contains? states
                                           val-keys)))
                     prepare-rules)))
         ;; Valid :invariant
         (fn [{:keys [states invariant] :as fsm}]
           (all-contains? states
                          (keys invariant)))))

;;;
;;; Example
;;;
(def state-example
  (let [a {:states #{:a :b}
           :rules {:a
                   {:b
                    {:action (fn [state] state)
                     :key (fn [state] true)}}}
           :invariant {:a (fn [state] true)}}]
    (assert (s/valid? :type.fsm/base a) "Not correct example")
    a))

