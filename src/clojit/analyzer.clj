(ns clojit.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require
    [clojure.tools.analyzer :as a]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]
    [clojure.tools.analyzer.utils :refer [resolve-var]]))

(defn desugar-host-expr [[op & expr :as form]]
  (if (symbol? op)
    (let [opname (name op)]
      (cond

       (= (first opname) \.) ; (.foo bar ..)
       (let [[target & args] expr
             args (list* (symbol (subs opname 1)) args)]
         (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) ia
                                      (first args) args)) ;; a method call or a field access
           (meta form)))

       (= (last opname) \.) ;; (class. ..)
       (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
         (meta form))

       :else form))
    form))

(defn macroexpand-1 [form env]
  (if (seq? form)
    (let [op (first form)]
      (if (a/specials op)
        form
        (let [v (resolve-var op env)]
          (if (and (not (-> env :locals (get op))) ;; locals cannot be macros
                   (:macro (meta v)))
            (apply v form env (rest form)) ; (m &form &env & args)
            (desugar-host-expr form)))))
        form))


(def e {:context :expr
        :locals {}
        :ns 'user
        :namespaces (atom
                     {'user {:mappings {}
                             :aliases {}
                             :ns 'user}
                      })})

(def foo 2)

(def e2 {:context :expr
        :locals {}
        :ns 'user
        :namespaces (atom
                     {'user {:mappings (into (ns-map 'clojure.core)
                                                     {'foo #'foo})
                                     :aliases {}
                                     :ns 'user}
                      'clojure.core {:mappings (ns-map 'clojure.core)
                                     :aliases {}
                                     :ns 'clojure.core}})})


(defmacro ast [form]
  `(binding [a/macroexpand-1 macroexpand-1
             a/create-var ~(fn [sym env]
                                  (doto (intern (:ns env) sym)
                                    (reset-meta! (meta sym))))
             a/parse a/-parse
             a/var? ~var?]
     (a/analyze '~form e2)))

(defmacro mexpand [form]
  `(macroexpand-1 '~form e2))

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



;; Are there children
;; else No children
;;   is constant

(defn creat-string-const-table [ast]

  (if (:children ast)
      (let [children-nodes (map (:children ast) ast)]
        (map creat-string-const-table children-nodes))
    (when (string-const? ast)
      (:val ast))))

#_(p/pprint (dissoc defa :meta ))

#_(defmethod generate identity)

#_(defmulti generate :def [node])

(p/pprint (dissoc (first (:args (ast (let [a 1] a)))) :meta))


(defn env-kick [node]
  (cond
   (map? node) (let [env-less-node (dissoc node :namespaces)]
                 (into {} (map (fn [[k v]] {k (env-kick v)}) env-less-node)))
   (vector? node)  (vec (map env-kick node))
   :default node))


(env-kick {:env {:local 1 :namespaces 1} :bla {:env 1 :bla 1} :blabla {:env 1 :bla 1}})

(p/pprint (env-kick (ast (let [a 1] a))))




