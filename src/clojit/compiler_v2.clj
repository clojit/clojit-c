(ns clojit.compiler-v2
  (:require
    [clojit.analyzer :as anal]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]
    [clojure.tools.trace :as t]
   ))

(declare ccompile find-constant-index)

(def sletplus (anal/ast (let [a 1] (+ a 2))))

(def empty-sourcmap {:line-counter 0 :bytecode []})

(def ^:dynamic *source-map* nil)

(def empty-constant-table
                     {:CSTR []
                      :CKEY []
                      :CINT []
                      :CFLOAT []})

(def constant-table (ref empty-constant-table))

(defn find-constant-index [op const]
  (first (remove nil? (map-indexed (fn [a b] (when (= b const)
                           a))
               (op @constant-table)))))

(defn put-in-constant-table [op const]
  (if (find-constant-index op const)
    @constant-table
    (dosync
     (alter constant-table assoc op (conj (op @constant-table) const)))))

(defn ADDVV [a-slot b-slot c-slot]
  {:op :ADDVV
   :a a-slot
   :b b-slot
   :c c-slot})

(defn JUMPF [a-slot d-slot]
  {:op :JUMPF
   :a a-slot
   :d d-slot})

(defn JUMP [d-slot]
  {:op :JUMP
   :d d-slot})



(defn constant-table-bytecode [bytecode a-slot const]
  {:op bytecode
   :a a-slot
   :d (find-constant-index bytecode const)})

(defn bool-bytecode [a-slot const]
  {:op :CBOOL
   :a a-slot
   :d (if const 1 0)})

(defn clojit_plus [node slot]
  (let [[arg1 arg2] (:args node)
        arg1_bytecode (ccompile arg1 slot)
        slot-1 (inc slot)
        arg2_bytecode (ccompile arg2 slot-1)]
    [arg1_bytecode
     arg2_bytecode
     (ADDVV slot slot slot-1)]))

(defmulti invoke (comp :var :fn))

(defmethod invoke #'+ [node slot]
    (clojit_plus node slot))









(defmulti ccompile :op)

(defmethod ccompile :invoke [node slot]
  [(invoke node slot)])

(defmethod ccompile :let [node slot]
  [(ccompile (:body node))])

(defmethod ccompile :do [node slot]
  [(ccompile (:ret node))])

;;-------------------------------------
;; 1..5   slot = test-bc             --
;; 6      jumpf slot (count then-bc) --
;; 7..10  slot = then-bc             --
;; 11     jump (count else- bc)      --
;; 12..15 slot = else-bc             --
;;-------------------------------------
(defmethod ccompile :if [node slot]
  (let [test-bc (flatten (ccompile (:test node) slot))
        then-bc (flatten (ccompile (:then node) slot))
        else-bc (flatten (ccompile (:else node) slot))]
    [test-bc
     (JUMPF slot (inc (count then-bc)))
     then-bc
     (JUMP (count else-bc))
     else-bc]))

(defmethod ccompile :const [node slot]
  (let [op
        (cond
         (anal/is-int? node) :CINT
         (anal/is-float? node) :CFLOAT
         (= :string (:type node)) :CSTR
         (= :keyword (:type node)) :CKEY
         (= :bool (:type node)) :CBOOL)]
    (if (= op :CBOOL)
      [(bool-bytecode slot (:val node))]
      (do (when-not (= op :CBOOL)
            (put-in-constant-table
             op
             (:val node)))
        [(constant-table-bytecode
         op
         slot
         (:val node))]))))



;; --------------------------- Function call -------------------------------------

(drop 1 (range))
(let [args [1 2 3 4]]
  )


(defn CALL [a-slot lit]
  [{:op :CALL :a-slot a-slot :d-slot lit}])



(range 5)

(defmethod invoke :default [node slot]
  (let [args (:args node)
        arg-slots (take (count args) (drop (+ 2 slot) (range)))
        arg-bc (mapcat ccompile args arg-slots)
        ]

    [arg-bc
     (ccompile (:fn slot) (inc slot))
     (CALL base (count args))
     ]
    )
  )




(defn clojit_plus [node slot]
  (let [[arg1 arg2] (:args node)
        arg1_bytecode (ccompile arg1 slot)
        slot-1 (inc slot)
        arg2_bytecode (ccompile arg2 slot-1)]
    [arg1_bytecode
     arg2_bytecode
     (ADDVV slot slot slot-1)]))



(def test-fn-ast (anal/ast (test-fn 1 2)))

(p/pprint (anal/env-kick test-fn-ast))

(def test-fn-bc (flatten (ccompile test-fn-ast 0)))


(p/pprint bc-ast-if)


;; --------------------------- ast-if -------------------------------------

(def ast-if (anal/ast (if (+ 1 1)
                        (+ 2 2)
                        (+ 3 3))))

(def bc-ast-if (flatten (ccompile ast-if 0)))

(p/pprint (anal/env-kick ast-if))
(p/pprint bc-ast-if)

;; --------------------------- bool -------------------------------------

(def bool-test (anal/ast (if true 1 9)))

(def bc-bool-test (flatten (ccompile bool-test 0)))

(p/pprint (anal/env-kick bc-bool-test))

;; --------------------------- constant-table -------------------------------------

(p/pprint @constant-table)

(defn set-empty []
  (dosync (alter constant-table (fn [ct] empty-constant-table))))

(set-empty)



