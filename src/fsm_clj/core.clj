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

(declare get-state-path eval-in-path)

;;
;; Utils
;;

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

(defn- get-all-edges
  [template state]
  (loop [edges (get-in template [state :edges])
         nstate state]
    (let [state-data (get template nstate)
          parent (:parent state-data)]
      (if (nil? parent)
        edges
        (let [parent-data (get template parent)
              parent-edges (:edges parent-data)]
          (recur (concat edges parent-edges)
                 parent))))))

;;
;; Creators
;;

(defn- unheir
  [template state]
  (loop [s state]
    (if (= :heir (:type (s template)))
      (recur (:start-state (s template)))
      s)))


(defn fsm
  [template start-state fsm-data]
  (let [state (unheir template start-state)
        in-states (get-state-path template state)
        new-data  (eval-in-path fsm-data
                                template
                                in-states)
        fsm {:template template
             :state state
             :data new-data}]
    fsm))

;;
;; Valid
;;

(defn fsm?
  [fsm]
  (nil? (s/check ftype/FSM fsm)))

;;
;; Update
;;

(defn- get-next-states
  [edges data]
  (filter #((:valid? %1) data) edges))

(defn- get-state-path
  [template state]
  (loop [cstate state
         path (list state)]
    (let [data (get template cstate)
          parent (:parent data)]
      (if (nil? parent)
        path
        (recur parent
               (conj path parent))))))

(defn- eval-key-path
  [template data path akey]
  (reduce (fn [data state]
            (let [afun (get-in template
                               [x akey])]
              (afun data)))
          data path))

(defn- eval-key-path
  [template data path akey]
  (reduce (fn [data state]
            (let [afun (get-in template
                               [x akey])]
              (afun data)))
          data path))

(defn- eval-out-action-path
  [data template path]
  (eval-path template data path :out-action))

(defn- eval-in-action-path
  [data template path]
  (eval-path template data path :in-action))

(defn- move-from-to
[{:keys [template data] :as fsm} from-state to-state]
(let [c-to-state (unheir template
to-state)
from-data (get template from-state)
to-data (get template c-to-state)
out-states (get-state-path template
from-state)
in-states (get-state-path template
c-to-state)
eq-count (->> (map vector out-states in-states)
(filter (fn [[x y]]
          (= x y)))
(count))
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
[{:keys [state data template] :as fsm} &
{:keys [choice-fn] :or {choice-fn first}}]
(let [action (get-in fsm
[:template state :action])
nfsm (if (nil? action)
fsm
(update-data fsm action))
fsm-data (:data nfsm)
template (:template nfsm)
edges (get-all-edges template state)
next-states (get-next-states edges fsm-data)]
(if (empty? next-states)
nfsm
(move-to nfsm (:state (choice-fn next-states))))))


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
(eval  `(fn ~'[state] ~(read-string code))))
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
