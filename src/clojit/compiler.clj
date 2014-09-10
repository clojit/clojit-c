(ns clojit.compiler
  (:require
    [clojit.analyzer :as anal]
    [clojit.bytecode-fn :as bcf]
    [clojit.bytecode-validation :as bcv]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]
    [clojure.tools.trace :as t]
    [schema.macros :as sm]
    ))

(declare ccompile)

;; ----------------------- MATH ----------------------------

(defn clojit_plus [node slot env]
  (let [[arg1 arg2] (:args node)
        arg1_bytecode (ccompile arg1 slot env)
        slot-1 (inc slot)
        arg2_bytecode (ccompile arg2 slot-1 env)]
    [arg1_bytecode
     arg2_bytecode
     (bcf/ADDVV slot slot slot-1)]))

;; ----------------------- INVOKE ----------------------------

(defmulti invoke (fn [node slot env] ((comp :var :fn) node)))

(defmethod invoke #'+ [node slot env]
    (clojit_plus node slot env))

(defmethod invoke :default [node slot env]
  (let [args (:args node)
        arg-slots (take (count args) (drop (+ 2 slot) (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))
        base (inc slot)]
    [arg-bc
     (ccompile (:fn node) base env)
     (bcf/CALL base (count args))]))

;; ----------------------- ccompile ----------------------------

(defmulti ccompile (fn [node slot env] (:op node)))

(defmethod ccompile :def [node slot env]
  (bcf/put-in-constant-table :CSTR (:name node))
  [(ccompile (:init node) slot env)
   (bcf/NSSETS slot (bcf/find-constant-index :CSTR (:name node)))])

(defmethod ccompile :invoke [node slot env]
  [(invoke node slot env)])

(defmethod ccompile :let [node slot env]
  (let [bindings (:bindings node)
        binding-slots (drop slot (range))
        new-env (apply merge (map (fn [b s] {(:name b) s})
                            bindings
                            binding-slots))
        merge-env (merge env new-env)]
    [(map ccompile bindings binding-slots (repeat merge-env))
     (ccompile (:body node) (+ slot (count bindings)) merge-env)]))

(defmethod ccompile :binding [node slot env]
  [(ccompile (:init node) slot env)])

(defn get-id [loop-id]
  (second (.split #"_" loop-id)))

(defmethod ccompile :fn [node slot env]
  (let [method  (first (:methods node))
        params  (:params method)
        local-env (apply merge (map-indexed (fn [i parm]
                                  {(:name parm) i})
                                            params))
        env (merge env local-env)
        argc    (count params)
        id      (Integer/parseInt (get-id (str (:loop-id method))))]
    (bcf/put-in-function-table
     id
     (vec (flatten [(bcf/FUNCF argc)
                    (ccompile (:body method) slot env)])))
    [(bcf/CFUNC slot id)]))

#_(c any-fn-test)

(defmethod ccompile :local [node slot env]
  (let [source (get env (:name node))]
    (bcf/MOV slot source)))

(defmethod ccompile :do [node slot env]
  [(map ccompile (:statments node) (repeat slot) (repeat env))
   (ccompile (:ret node) slot env)])

(defmethod ccompile :maybe-class [node slot env]
  [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (:class node)))])

;;-------------------------------------
;; 1..5   slot = test-bc             --
;; 6      jumpf slot (count then-bc) --
;; 7..10  slot = then-bc             --
;; 11     jump (count else- bc)      --
;; 12..15 slot = else-bc             --
;;-------------------------------------
(defmethod ccompile :if [node slot env]
  (let [test-bc (flatten (ccompile (:test node) slot env))
        then-bc (flatten (ccompile (:then node) slot env))
        else-bc (flatten (ccompile (:else node) slot env))]
    [test-bc
     (bcf/JUMPF slot (inc (count then-bc)))
     then-bc
     (bcf/JUMP (count else-bc))
     else-bc]))

(defmethod ccompile :const [node slot env]
  (let [op
        (cond
         (anal/is-int? node) :CINT
         (anal/is-float? node) :CFLOAT
         (= :string (:type node)) :CSTR
         (= :keyword (:type node)) :CKEY
         (= :bool (:type node)) :CBOOL)]
    (if (= op :CBOOL)
      [(bcf/bool-bytecode slot (:val node))]
      (do (when-not (= op :CBOOL)
            (bcf/put-in-constant-table
             op
             (:val node)))
        [(bcf/constant-table-bytecode
         op
         slot
         (:val node))]))))


;; ----------------------- main compile ----------------------------

(defn print-node [node]
  (p/pprint (dissoc (anal/env-kick node) :meta)))

(defn c0 [node]
  (vec (flatten (ccompile node 0 {}))))

(defn c [node]
  (print-node node)
  (let [bc (c0 node)]
    (p/pprint bc)
    (p/pprint @bcf/constant-table)
    bc))
;; ----------------------- file output --------------------------------

(sm/defn ^:always-validate
  gen-bytecode-output-data :- bcv/Bytecode-Output-Data [bc :- bcv/Bytecode-List]
    (let [bytecode-output (assoc-in @bcf/constant-table [:CFUNC 0] bc)]
      (bcf/set-empty)
      bytecode-output))

(sm/defn ^:always-validate
  gen-file-output [bc :- bcv/Bytecode-List]
  (let [bytecode-output (gen-bytecode-output-data bc)
        bytecode-output-json (with-out-str (json/pprint bytecode-output))]
    (println "Bytecode Output Clojure: ")
    (p/pprint bytecode-output)
    (spit "clojure-bc.json" bytecode-output-json)))



;; --------------------------- gen-file-output ---------------------------------

#_(gen-bytecode-output-data (c0 (anal/ast (+ 1 1))))

#_(gen-file-output (c0 (anal/ast (fn [a b] (+ a b)))))

;; --------------------------- any fn ---------------------------------

#_(anal/env-kick (anal/ast (fn [a] a)))

#_(def any-fn-test (anal/ast (do (fn [a] a) (fn [b] (+ b 1)))))

#_(p/pprint (anal/env-kick any-fn-test))

#_(bcf/set-empty)

#_(c any-fn-test)

;; --------------------------- def ---------------------------------

#_(def def-test (anal/ast (def x 1)))

#_(print-node def-test)

#_(c def-test)

;; --------------------------- lookup -----------------------------

#_(def invoke-test (anal/ast (my-plus 1 2 3 4)))

#_(c invoke-test)

;; --------------------------- let ---------------------------------

#_(def let-test (anal/ast (let [a 1] (if true (+ a a) (+ 10 a)))))

#_(print-node let-test)
#_(print-node (:bindings let-test))
#_(print-node (:body let-test))

#_(print-node (:ret (:body let-test)))

#_(print-node   (:body let-test))

#_(p/pprint (c let-test))

;; --------------------------- Do -------------------------------------

#_(def do-test (anal/ast (do 1 2)))

#_(print-node do-test)

;; --------------------------- Function call -------------------------------------

#_(def test-fn-ast (anal/ast (test-fn 1 2)))

#_(p/pprint (anal/env-kick test-fn-ast))

#_(def test-fn-bc (c test-fn-ast))

#_(p/pprint bc-ast-if)

;; --------------------------- ast-if -------------------------------------

#_(def ast-if (anal/ast (if (+ 1 1)
                        (+ 2 2)
                        (+ 3 3))))

#_(def bc-ast-if (flatten (ccompile ast-if 0 env)))

#_(p/pprint (anal/env-kick ast-if))
#_(p/pprint bc-ast-if)


;; --------------------------- bool -------------------------------------

#_(def bool-test (anal/ast (if true
                           (+ 1 6) 9)))

#_(def bc-bool-test (flatten (ccompile bool-test 0 {})))

#_(p/pprint bc-bool-test)

#_(print-node bool-test)

#_(p/pprint (anal/env-kick bc-bool-test))


#_(print-node (anal/ast (+ 1 1)))

#_(p/pprint (c (anal/ast (+ 1 1))))
#_(p/pprint @bcf/constant-table)

;; --------------------------- def -------------------------------------

#_(def def-test (anal/ast (def a 1)))

#_(keys def-test)

#_(:name def-test)

#_(:op def-test)

#_(print-node (dissoc def-test :meta ))


