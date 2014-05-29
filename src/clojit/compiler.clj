(ns clojit.compiler
  (:require
    [clojit.analyzer :as anal]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]))

(def splus (anal/ast (+ 1 2)))

(def sletplus (anal/ast (let [a 1] (+ a 2))))

(def ccompile 1)

(def constant-table (ref {}))

(defn clean-const [const-node]
  (dissoc const-node
          :env
          :number
          :literal?
          :form
          :op))

(defn add-const-type [const-node]
  (cond
   (anal/is-int? const-node) (assoc const-node :type :int)
   (anal/is-float? const-node) (assoc const-node :type :float)
   (anal/keyword-const? (assoc const-node :type :keyword))
   (anal/string-const? (assoc const-node :type :string))))


(defn check-double [const-node constant-table]
  (first (filter (comp not nil?) (map (fn [[index const-entry]]
                                        (when (= (clean-const (add-constant-type const-node))
                                                 const-entry)
                                          index))
                                      @constant-table))))

#_(check-double {:type :num :val 1} {0 {:type :int :val 1} 1 {:type :float :val 1.0}  2 {:type :string :val "test"}})

(defn creat-constant-table-entery [constant-node constant-table]
  (let [potential-const (clean-const (add-constant-type constant-node))]
    (dosync
     (let [double-index (check-double constant-node constant-table)]
       (if double-index
         double-index
         (let [table-count (count @constant-table)]
           (do (alter constant-table
                      assoc table-count potential-const)
             table-count)))))))


(defn local-resolve [var-name env]
  (let [val (:init ((:locals env) var-name))]
    (if (= (:op val) :const)
      (let [const-index (creat-constant-table-entery val constant-table)]
        {:type :const
         :index const-index})
      {:type :unknown})))


(defmulti clojit_plus (fn [node]
                        (vec (map :op (:args node)))))

(defmethod clojit_plus [:const :const] [node]

  (let [args (:args node)]
    (when (every? #(= :number %)
                (map :type args))
      (let [[arg1-const-index arg2-const-index] (map #(creat-constant-table-entery % constant-table) args)]
        [(CNUM 0 arg1-const-index)
         (CNUM 1 arg2-const-index)
         (ADDVV 2 0 1)]))))

(defmethod clojit_plus [:local :const] [node]
  (let [args (:args node)
        local (first args)
        const (second args)]
    [
     (let [resolved-local (local-resolve (:name local) (:env local))]
       (if (= (:type resolved-local :const))
         (CNUM 0 (:index resolved-local))
         {:type :unkown})
       )
     (CNUM 1 (creat-constant-table-entery const constant-table))
     (ADDVV 2 0 1)]))

(defn CNUM [slot const-index]
  {:op (if (= :int (:type (@constant-table const-index)))
         :CINT
         :CFLOAT)
   :a slot
   :b const-index})


(defn ADDVV [a-slot b-slot c-slot]
  {:op :ADDVV
   :a a-slot
   :b b-slot
   :c c-slot})


(defmulti invoke (comp :var :fn))

(defmethod invoke #'+ [node]
    (clojit_plus node))


(defmulti ccompile :op)

(defmethod ccompile :invoke [node]
  (invoke node))

(defmethod ccompile :let [node]
  (ccompile (:body node)))

(defmethod ccompile :do [node]
  (ccompile (:ret node)))

(p/pprint constant-table)

#_(p/pprint (ccompile splus))

(p/pprint (ccompile sletplus))







