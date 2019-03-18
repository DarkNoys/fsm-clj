(ns fsm-clj.type
  (:require
   [cheshire.core :as json]
   [schema.coerce :as coerce]
   [schema.experimental.abstract-map :as amap]
   [schema.core :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iside fsm structure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def State
  s/Keyword)

(defn CustomStateEdge
  [ & {:keys [fn-type] :or {fn-type (s/=> s/Any)}}]
  {(s/optional-key :discription) s/Str
   :state State
   :valid? fn-type})

(def StateEdge
  (CustomStateEdge :fn-type (s/=> s/Any)))

(defn CustomNotTypedStateDifinition
  [ & {:keys [fn-type] :or {fn-type (s/=> s/Any)}}]
  {(s/optional-key :edges) [(CustomStateEdge :fn-type fn-type)]
   (s/optional-key :action) fn-type
   (s/optional-key :in-action) fn-type
   (s/optional-key :out-action) fn-type
   (s/optional-key :discription) s/Str
   (s/optional-key :parent) State})

(def NotTypedStateDifinition
  (CustomNotTypedStateDifinition :fn-type (s/=> s/Any)))

(s/defschema StateDefinition
  (amap/abstract-map-schema
   :type
   NotTypedStateDifinition))

(amap/extend-schema NormalState
                    StateDefinition
                    [:normal]
                    {})

(amap/extend-schema HeirState
                    StateDefinition
                    [:heir]
                    {:start-state State})

(def Template
  {s/Keyword
   StateDefinition})

(def FSM
  {(s/optional-key :discription) s/Str
   :template Template
   :state State
   :data s/Any})

;;;;;;;;;;;;;;;;;
;; JSON Schema ;;
;;;;;;;;;;;;;;;;;

(declare JsonTemplate)

(def JsonFnVar
  (s/pred
   (fn [item]
     (and
      (string? item)
      (not (nil? (re-matches
                  #"^#'[^\s@;#$%\\/0-9^][^\s@;#$%\\/^]*\/[^\s@;#$%\\/0-9^][^\s@;#$%\\/^]*$"
                  item)))))))

(def JsonFn
  [(s/one
    JsonFnVar
    "fun")
   (s/optional
    s/Any
    "param")])

(def JsonNotTypedStateDifinition
  (CustomNotTypedStateDifinition :fn-type JsonFn))

(s/defschema JsonStateDefinition
  (amap/abstract-map-schema
   :type
   JsonNotTypedStateDifinition))

(amap/extend-schema JsonNormalState
                    JsonStateDefinition
                    [:normal]
                    {})

(amap/extend-schema JsonHeirState
                    JsonStateDefinition
                    [:heir]
                    {:start-state State
                     :fsm (s/recursive #'JsonTemplate)})

(def JsonTemplate
  {s/Keyword
   JsonStateDefinition})

(def JsonFSM
  {(s/optional-key :discription) s/Str
   :template JsonTemplate
   :start-state State
   :init-data s/Any})
