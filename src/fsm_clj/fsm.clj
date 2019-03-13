(ns fsm-clj.fsm
  (:require
   [clojure.spec.alpha :as s]))


;;;;;;;;;;;;;;;;;;;;;;
;;; Base namespace ;;;
;;;;;;;;;;;;;;;;;;;;;;

(declare get-state-path eval-in-path)

;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;

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

(defn- unique?
  [lst]
  (= (count lst) (count (distinct lst))))

;;;;;;;;;;;;;;;;
;;; Creators ;;;
;;;;;;;;;;;;;;;;

;;;
;;; Template
;;;

(s/def ::symbol-fun #(fn? (eval %)))
(s/def ::tschema-line-actions
  (s/&
   (s/*
    (s/cat :action #{:in-action :action :out-action}
           :fun ::symbol-fun))
   (fn [lst] (<= (count lst) 3))
   (fn [lst] (let [n (count lst)
                   set-n (count (set (map #(:action %) lst)))]
               (= n set-n)))))
(s/def ::tschema-egdes-valid
  (s/&
   (s/*
    (s/alt :valid? (s/cat :name #{:valid?}
                          :value ::symbol-fun)
           :discription (s/cat :name #{:discription}
                               :value string?)))
   (fn [lst] (<= (count lst) 2))
   (fn [lst] (let [n (count lst)
                   set-n (count (set (map #(:type %) lst)))]
               (= n set-n)))))
(s/def ::tschema-egdes (s/cat :arrow #(= (quote =>) %)
                              :state keyword?
                              :valid? ::tschema-egdes-valid))
(s/def ::tschema-line (s/cat :state keyword?
                             :actions ::tschema-line-actions
                             :schema (s/alt :normal ::tschema-egdes
                                            :heir ::tschema)))
(s/def ::tschema (s/coll-of ::tschema-line :kind vector? :into []))

(defn- parse-pre-template
  [pre-temp]
  pre-temp)
(defmacro parse-tschema
  [schema]
  `(let [pre-temp# (s/conform ::tschema (quote ~schema))]
     (parce-pre-template pre-temp#)))

(defn valid-template-schema
  "Valid template-schema to correct"
  [template])

(defn- parse-template-schema-line
  [schema-line]
  [])
(defn- parse-template-schema
  "Parse schema to FSM template.

   Schema format:
   [[:a :action (fn [data] ...)
        :in-action (fn [data] ...)
        :out-action (fn [data] ...)
        -> :b :discription \"dsaf\"
              :valid? (fn [data] ...)
        -> :c :discription \"dsaf\"
              :valid? (fn [data] ...)
        ...]
    [:c :action (fn [data] ...)
        :in-action (fn [data] ...)
        :out-action (fn [data] ...)
        [[:r ...]]]
    ...]"
  [schema]
  (reduce
   parse-template-schema-line
   {}
   schema))


(defn make-template
  [template-schema]
  (assert (valid-template-schema template-schema))
  (parse-template-schema template-schema))

;; FIX exception
(defn valid-template
  [template]
  (and (all (map (fn [[_ v]] (and
                              (contains? v :type)
                              (if (= (:type v) :heir)
                                (contains? v :start-state)
                                true)
                              (contains? v :edges)
                              (all (map #(and (contains? % :state)
                                              (contains? % :valid?))
                                       (:edges v)))))
                 template))
       (let [states (set (keys template))
             in-states (set (reduce concat
                                    (map (fn [[_ v]]
                                           (concat (if (= (:type v) :heir)
                                                     (list (:start-state v))
                                                     ())
                                                   (map #(:state %)
                                                        (:edges v))))
                                         template)))]
         (all-contains states in-states))))

(defn valid-fsm
  [{:keys [template state data states] :as fsm}]
  (and
   (keyword? state)
   (= states (keys template))
   (valid-template template)))

(defn unheir
  [template state]
  (loop [s state]
    (if (= :heir (:type (s template)))
      (recur (:start-state (s template)))
      s)))


(defn make-fsm
  [template start-state fsm-data]
  (let [state (unheir template start-state)
        in-states (get-state-path template state)
        new-data  (eval-in-path fsm-data
                                template
                                in-states)
        fsm {:template template
             :state state
             :data new-data
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

(defn get-all-edges
  [template state]
  (loop [edges (:edges (get template state))
         nstate state]
    (let [state-data (nstate template)
          parent (:parent state-data)]
      (if (nil? parent)
        edges
        (let [parent-data (parent template)
              parent-edges (:edges parent-data)]
          (recur (concat edges parent-edges) parent))))))
;;;
;;; Update
;;;

(defn- get-next-states
  [edges data]
  (filter #((:valid? %1) data) edges))

(defn- get-state-path
  [template state]
  (loop [cstate state
         path (list state)]
    (let [data (cstate template)
          parent (:parent data)]
      (if (nil? parent)
        path
        (recur parent (conj path parent))))))

(defn- eval-path
  [template data path ekey]
  (loop [ndata data
         [x & xl] path]
    (if (nil? x)
      ndata
      (let [f (ekey (x template))]
        (if (nil? f)
          (recur ndata xl)
          (recur (f ndata) xl))))))

(defn- eval-out-path
  [data template path]
  (eval-path template data path :out-action))

(defn- eval-in-path
  [data template path]
  (eval-path template data path :in-action))

(defn- move-from-to
  [{:keys [template data] :as fsm} from-state to-state]
  (let [c-to-state (unheir template to-state)
        from-data (from-state template)
        to-data (c-to-state template)
        out-states (get-state-path template from-state)
        in-states (get-state-path template c-to-state)
        eq-count (count (filter (fn [[x y]]
                                  (= x y))
                                (map vector out-states in-states)))
        new-data (-> data
                     (eval-out-path template
                                    (take-last
                                     (- (count out-states)
                                        eq-count)
                                     out-states))
                     (eval-in-path template
                                   (take-last
                                    (- (count in-states)
                                       eq-count)
                                    in-states)))]
    (println in-states)
    (println out-states)
    (println eq-count)
    (println (take-last
              (- (count in-states)
                 eq-count)
              in-states))
    (-> fsm
        (assoc :data new-data)
        (assoc :state c-to-state))))

(defn move-to
  [{:keys [template data state] :as fsm} to-state]
  (move-from-to fsm state to-state))

(defn update-data
  [fsm updater]
  (update fsm :data updater))

(defn update-fsm
  [fsm & {:keys [choice-fn]
          :or {choice-fn first}}]
  (let [state (get-state fsm)
        action (get-in fsm [:template state :action])
        nfsm (if (nil? action)
               fsm
               (update-data fsm action))
        fsm-data (get-data nfsm)
        template (get-template nfsm)
        edges (get-all-edges template state)
        next-states (get-next-states edges fsm-data)]
    (if (empty? next-states)
      nfsm
      (move-to nfsm (:state (choice-fn next-states))))))
