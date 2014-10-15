(ns clojit.analyzer
  (:require
    [clojure.tools.analyzer.jvm :as ana]
    [clojure.tools.analyzer.utils :as u]
    [clojure.pprint :as p]))


(defn ast [form]

  (let [a (ana/analyze form)]

    a))

(defn asteval [form]

  (let [a
        (ana/analyze+eval form)]

    a))

(defn env-kick [node]
  (cond
   (map? node) (let [env-less-node (dissoc node :namespaces :init)]
                 (into {} (map (fn [[k v]] {k (env-kick v)}) env-less-node)))
   (vector? node) (mapv env-kick node)
   :default node))

(defn dissoc-env [ast]
  (u/dissoc-env ast))

(defn in-const-table? [ast-node ast-node-filter-type]
  (and (= (:op ast-node) :const)
       (= (:type ast-node) ast-node-filter-type)
       (:literal? ast-node)))

(defn number-const? [ast-node]
  (in-const-table? ast-node :number))

(defn string-const? [ast-node]
  (in-const-table? ast-node :string))

(defn keyword-const? [ast-node]
  (in-const-table? ast-node :keyword))

(defn is-int? [ast-node]
  (integer? (:val ast-node)))

(defn is-float? [ast-node]
  (float? (:val ast-node)))



