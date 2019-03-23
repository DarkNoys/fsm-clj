(ns fsm-clj.core
  (:require
   [fsm-clj.type :as ftype]
   [schema.core :as s]
   [schema.coerce :as c]
   [cheshire.core :as json]))

(defn- update-in-vals-if-exist
  [item vals fun]
  (reduce (fn [nitem val]
            (if (nil? (get-in nitem val))
              nitem
              (update-in nitem val fun)))
          item vals))

;;;;;;;;;
;; FSM ;;
;;;;;;;;;

;;
;; Utils
;;

(defn- state-heir-path
  "Return all parents of state"
  [{:keys [template] :as fsm} state]
  (loop [cstate state
         path (list state)]
    (let [data (get template cstate)
          parent (:parent data)]
      (if (nil? parent)
        path
        (recur parent
               (conj path parent))))))

(defn- eval-actions
  ([{:keys [state] :as fsm} ekey]
   (eval-actions
    fsm
    (state-heir-path
     fsm state)
    ekey))
  ([{:keys [data template] :as fsm}
    path ekey]
   (assoc fsm
          :data
          (reduce
           (fn [ndata x]
             (let [f (get-in template
                             [x ekey])]
               (if (nil? f)
                 data
                 (f ndata))))
           data path))))

(defn- current-edges
  "Return all edges in current state and all parent of this state"
  [{:keys [template state] :as fsm}]
  (reduce
   (fn [edges state]
     (let [nedges (get-in template [state :edges])]
       (if (nil? nedges)
         edges
         (concat edges nedges))))
   (get-in template [state :edges])
   (state-heir-path fsm state)))

(defn- unheir
  "Return first not heir state in state node"
  [template state]
  (loop [s state]
    (if (= :heir (:type (s template)))
      (recur (:start-state (s template)))
      s)))

;;
;; Creators
;;

(defn fsm
  [template start-state fsm-data]
  (let [state (unheir template start-state)
        fsm {:template template
             :state state
             :data fsm-data}]
    (assert (nil? (get-in template [start-state :parent])) "Start state in othre state")
    (eval-actions fsm
                  :in-action)))

;;
;; Valid
;;

(defn fsm?
  [fsm]
  (nil?
   (s/check
    ftype/FSM fsm)))

;;
;; Update
;;

(defn update-data
  "Update data in FSM.
   Return new FSM."
  [fsm updater]
  (update fsm :data updater))

(defn- get-next-states
  [edges data]
  (filter
   #((:valid? %1)
     data)
   edges))

(defn- unheir-path-from-to
  [{:keys [template data state] :as fsm} from-state to-state]
  (let [out-states (state-heir-path template
                                        from-state)
        in-states (state-heir-path template
                                       to-state)
        eq-count (->> (map vector out-states in-states)
                      (filter (fn [[x y]]
                                (= x y)))
                      (count))]
    [(take-last
      (- (count out-states)
         eq-count)
      out-states)
     (take-last
      (- (count in-states)
         eq-count)
      in-states)]))

(defn move-state
  ([{:keys [template data state] :as fsm} to-state]
   (let [real-to-state (unheir template
                               to-state)
         [out-states in-states] (unheir-path-from-to
                                 fsm state real-to-state)
         new-data (-> data
                      (eval-actions fsm
                                    out-states
                                    :out-action)
                      (eval-actions fsm
                                    out-states
                                    :in-action))]
     (-> fsm
         (assoc :data new-data)
         (assoc :state real-to-state)))))


(defn update-fsm
  [{:keys [state template] :as fsm} & {:keys [choice-fn]
                                       :or {choice-fn first}}]
  (let [nfsm (eval-actions fsm
                           :action)
        fsm-data (:data nfsm)
        edges (current-edges nfsm)
        next-states (get-next-states edges fsm-data)]
    (if (empty? next-states)
      nfsm
      (move-to nfsm
               (:state
                (choice-fn next-states))))))


;;;;;;;;;;;;
;; Import ;;
;;;;;;;;;;;;

(defn allow-ns?
  [var-ns rules]
  (cond
    (nil? rules) true
    (keyword? rules) (-> var-ns
                         (meta)
                         rules
                         (nil?)
                         (not))
    :else false))


(defn- parse-fn
  [code & {:keys [allow-ns]}]
  (eval  `(fn ~'[state]
            ~(read-string code))))

#_(defn- parse-fn
    [[name & args] & {:keys [allow-ns]}]
    (let [[_ ns-name fn-name] (re-find
                               #"^#'([^\s@;#$%\\/0-9^][^\s@;#$%\\/^]*)\/([^\s@;#$%\\/0-9^][^\s@;#$%\\/^]*)$"
                               name)
          var-ns (find-ns (symbol ns-name))
          var-fn (get (ns-publics (symbol ns-name))
                      (symbol fn-name))]
      (assert (not (nil? var-ns)))
      (assert (not (nil? var-fn)))
      (assert (allow-ns? var-ns allow-ns))
      (fn [state]
        (apply (partial var-fn state)
               args))))

;;
;; Json
;;

(declare parse-json-template)

(defn- parse-json-state-data-type-update
  [template state & {:keys [allow-ns parent]}]
  (let [stype (get-in template
                      [state :type])]
    (condp = stype
      :heir (merge
             template
             (parse-json-template
              (get-in template
                      [state :fsm])
              :allow-ns allow-ns
              :parent state))
      :normal template
      (throw (Exception. "Not valid type")))))

(defn- parse-json-state-data
  [template state & {:keys [allow-ns parent]}]
  (-> template
      (parse-json-state-data-type-update
       state
       :allow-ns allow-ns
       :parent parent)
      (assoc-in [state :parent] parent)
      (update-in-vals-if-exist [[state :edges]]
                               (fn [edges]
                                 (mapv (fn [edge]
                                         (update
                                          edge :valid?
                                          #(parse-fn % :allow-ns allow-ns)))
                                       edges)))
      (update-in-vals-if-exist [[state :action]
                                [state :out-action]
                                [state :in-action]]
                               #(parse-fn % :allow-ns allow-ns))))

(defn- parse-json-template
  [template & {:keys [allow-ns parent]}]
  (loop [ntemplate template
         [state & states] (keys template)]
    (if (nil? state)
      ntemplate
      (recur (parse-json-state-data
              ntemplate
              state
              :allow-ns allow-ns
              :parent parent)
             states))))

(defn- json-to-fsm
  [{:keys [discription init-data
           start-state template]
    :as json-data} & {:keys [allow-ns]}]
  (fsm
   (parse-json-template
    template
    :allow-ns allow-ns)
   start-state
   init-data))

(defn- load-json
  [data & {:keys [allow-ns]}]
  (let [json-data ((c/coercer ftype/JsonFSM
                              c/json-coercion-matcher)
                   data)]
    (assert (nil? (:error json-data)) (format "Not correct file format %s" (print-str json-data)))
    (json-to-fsm json-data :allow-ns allow-ns)))

(defn load-json-string
  [string & {:keys [allow-ns]}]
  (let [data (json/parse-string
              string
              true)]
    (load-json data :allow-ns allow-ns)))

(defn load-json-file
  "
  Return fsm object from file.

  Params:

  - **file-reader*** - java.io.Reader

  "
  [file-reader & {:keys [allow-ns]}]
  (let [data (json/parse-stream
              file-reader
              true)]
    (load-json data :allow-ns allow-ns)))
