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
     (a/analyze ~form e2)))


(defn fast [form]
  (ast form))


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

(defn is-float? [ast-node]
  (float? (:val ast-node)))

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

#_(p/pprint (dissoc (first (:args (ast (let [a 1] a)))) :meta))


(defn env-kick [node]
  (cond
   (map? node) (let [env-less-node (dissoc node :namespaces :init)]
                 (into {} (map (fn [[k v]] {k (env-kick v)}) env-less-node)))
   (vector? node) (mapv env-kick node)
   :default node))


#_(env-kick {:env {:local 1 :namespaces 1} :bla {:env 1 :bla 1} :blabla {:env 1 :bla 1}})

#_(p/pprint (env-kick (ast (let [a 1] a))))


#_(def mp (env-kick (ast
   (loop [a 1 b 2]
     (if (= a 10)
      a
      (recur (inc b) (inc a)))))))


#_(def lplus (env-kick (ast (let [a (+ 1 3)] (+ a 2)))))

#_(p/pprint lplus)

#_(def afn (env-kick (ast (fn ([a] (inc a)) ))))

#_(ast (def myfuna (fn [a] a)))

#_(p/pprint (env-kick (ast (defn abc [a] a))))

#_(p/pprint mp)

#_(keys mp) ;; (:op :form :env :loop-id :body :bindings :children)

#_(:op mp) ;; :loop

#_(:form mp) ;; (loop* [a 1] (if (= a 10) a (recur (inc a))))

#_(:env mp) ;; {:loop-id loop_7145, :context :expr, :locals {}, :ns user}

#_(println (:loop-id mp)) ;; loop_7145

#_(println (:body mp))

#_(println (:bindings mp)) ;; [{:op :binding,
                         ;;   :env {:loop-id loop_7145, :context :expr, :locals {}, :ns user},
                         ;;   :name a,
                         ;;   :init  {:op :const, :env {:loop-id loop_7145, :context :expr, :locals {}, :ns user},
                                 ;;   :type :number,
                                 ;;   :literal? true,
                                 ;;   :val 1, :form 1},
                                 ;;   :form a,
                                 ;;   :local :loop,
                                 ;;   :children [:init]}]






