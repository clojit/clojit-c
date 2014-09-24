(ns clojit.env
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]
    [schema.core :as s]
    [schema.macros :as sm]))


(declare env)

(def parent-env
  "A schema for validation of the parent env"
  {(s/optional-key :parent) env})

(def lvl1-env
  "A schema for validation of the env"
  {(s/maybe s/Str) {(s/required-key :slot) s/Int
                    (s/optional-key :freevar) s/Int}})

(def env
  "A schema for validation of the full-env"
  (merge lvl1-env parent-env))


(def freevar-env
  {:freevar s/Int})

(def slot-env
  {:slot s/Int})

(def get-env-schema
  (s/either freevar-env slot-env))

(sm/defn ^:always-validate get-id :- s/Str
  [loop-id :- s/Str]
  (second (.split #"_" (str loop-id))))



(defn convert-to-freevar
  [normal-env]
  (let [parent-free-env (dissoc normal-env :parent)
        new-freevar (if-not (contains? env :parent)
                      0
                      (inc (apply max
                                  (map :freevar
                                       (vals (:parent normal-env))))))]
    (apply merge (map (fn [i [var-name slot]]
                        (if (= var-name :parent)
                          {:parent slot}
                          {var-name (assoc slot
                                     :freevar i)}))
                      (drop new-freevar (range))
                      normal-env))))

(defn has-fn-subnode? [node]
  (cond
   (map? node) (if (= (:op node) :fn)
                 true
                 (some true? (map (fn [[k v]] (if (or (map? v) (vector? v))
                                                      (has-fn-subnode? v))) node)))
   (vector? node) (some true? (mapv has-fn-subnode? node))
   :default false))


(comment  get-env-schema
  )


(defn ^:always-validate get-in-parent
  [env name ]
  (when env
    (dissoc (get env name) :slot)))

^:always-validate

(comment  get-env-schema)

(defn  get-env
  [env name]
  (if-let [local (get env name)]
    local
    (get-in-parent (:parent env) name)))
