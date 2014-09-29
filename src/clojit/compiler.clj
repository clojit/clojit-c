(ns clojit.compiler
  (:require
    [clojit.analyzer :as anal]
    [clojit.bytecode-fn :as bcf]
    [clojit.bytecode-validation :as bcv]
    [clojit.visualiser :as v]
    [clojit.env :as e]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]
    [clojure.tools.trace :as t]
    [schema.macros :as sm]))

(declare ccompile)



;; ----------------------- INVOKE --- Math ----------------------

(defn binop-reduce
  [slot env op acc arg]
    (let [arg-slot (inc slot)
          arg-bc (ccompile arg arg-slot env)]
    [acc arg-bc (op slot slot arg-slot)]))

(defn binop [op node slot env]
  (let [[first-arg & args] (:args node)
        first-bc (ccompile first-arg slot env)
        reducer (partial binop-reduce slot env op)]
    (flatten (reduce reducer first-bc args))))

(defn neutralbinop [op neutral node slot env]
  (if (empty? (:args node))
    (do
      (bcf/put-in-constant-table :CINT neutral)
      (bcf/constant-table-bytecode :CINT slot neutral))
    (binop op node slot env)))

(defmulti invoke (fn [node slot env] ((comp :var :fn) node)))

(defmethod invoke #'+ [node slot env]
  (neutralbinop bcf/ADDVV 0 node slot env))

(defmethod invoke #'* [node slot env]
  (neutralbinop bcf/MULVV 1 node slot env))

(defmethod invoke #'- [node slot env]
  (let [args (:args node)]
    (if (= 1 (count args))
      (conj (ccompile (first args) slot env) (bcf/NEG slot slot))
      (binop bcf/SUBVV node slot env))))

(defmethod invoke #'/ [node slot env]
  (let [args (:args node)]
    (if (> 1 (count args))
      (binop bcf/DIVVV node slot env)
      (let [one-slot slot
            one-bc (do
                     (bcf/put-in-constant-table :CINT 1)
                     (bcf/constant-table-bytecode :CINT one-slot 1))
            arg (first args)
            arg-slot (inc slot)
            arg-bc (ccompile arg arg-slot env)
            div-bc (bcf/DIVVV slot one-slot arg-slot)]
        (flatten [one-bc arg-bc div-bc])))))


;; ----------------------- INVOKE --- NOT ----------------------

(defmethod invoke #'not [node slot env]
  (let [arg (first (:args node))
        arg-bc (ccompile arg slot env)]
    [arg-bc
     (bcf/NOT slot slot)]))

;; ----------------------- INVOKE --- Array ----------------------

(defmethod invoke #'aset [node slot env]
  (let [args (:args node)
        arg-slots (take (count args) (drop slot (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (apply bcf/SETARRAY arg-slots)]))

(defmethod invoke #'aget [node slot env]
  (let [args (:args node)
        arg-slots (take (count args) (drop slot (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/GETARRAY slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'make-array [node slot env]
  (let [args (:args node)
        arg-slots (take (count args) (drop slot (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/NEWARRAY slot (first arg-slots))]))


;; ----------------------- INVOKE --- Comparisant ----------------------

(comment
    ISLT	dst var	var	    A = B < C
    ISGE	dst var	var     A = B ≥ C
    ISLE	dst var var	    A = B ≤ C
    ISGT	dst var var	    A = B > C
    ISEQ	dst var var	    A = B == C
    ISNEQ	dst var var	    A = B ≠ C)

(defmethod invoke #'< [node slot env]
  (bcf/ISLT))

(defmethod invoke #'>=  [node slot env]
  (bcf/ISGE))

(defmethod invoke #'>=  [node slot env]
  (bcf/ISLE))

(defmethod invoke #'>  [node slot env]
  bcf/ISGT)

(defmethod invoke #'==  [node slot env]
  bcf/ISEQ)

(defmethod invoke :default [node slot env]
  (let [args (:args node)
        base slot
        arg-count (count args)
        arg-slots (take arg-count (drop (+ 2 base) (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))
        func-slot (inc base)
        lit (inc arg-count)]
    [arg-bc
     (ccompile (:fn node) func-slot env)
     (bcf/CALL base lit)]))

;; ----------------------- INVOKE -------------------------

(defmulti ccompile (fn [node slot env] (:op node)))

(defmethod ccompile :def [node slot env]
  (bcf/put-in-constant-table :CSTR (str (:name node)))
  [(ccompile (:init node) slot env)
   (bcf/NSSETS slot (bcf/find-constant-index :CSTR (str (:name node))))])

(defmethod ccompile :invoke [node slot env]
  [(invoke node slot env)])

(defmethod ccompile :let [node slot env]
  (let [bindings (:bindings node)
        binding-slots (drop slot (range))
        new-env (apply merge (map (fn [b s] {(str (:name b)) {:slot s}})
                                  bindings
                                  binding-slots))
        merge-env (merge env new-env)]
    [(map ccompile bindings binding-slots (repeat merge-env))
     (ccompile (:body node) (+ slot (count bindings)) merge-env)
     (bcf/MOV slot (+ slot (count bindings)))]))

(defmethod ccompile :binding [node slot env]
  [(ccompile (:init node) slot env)])

(defmethod ccompile :fn [node slot env]
  (let [method  (first (:methods node))
        params  (:params method)
        args-slots (drop (inc slot) (range))
        local-env (apply merge (map (fn [i parm]
                                      {(str (:name parm)) {:slot i}})
                                    args-slots
                                    params))
        freevar-env (e/convert-to-freevar env)
        parent-freevar-env (if freevar-env
                             (merge {:parent freevar-env} local-env)
                             local-env)
        has-fn-subnode (e/has-fn-subnode? (:methods node))
        argtc   (count params)
        argc    (:fixed-arity method)
        id      (Integer/parseInt (e/get-id (str (:loop-id method))))]
    (bcf/put-in-function-table
     id
     (vec (flatten [(if (:variadic? method)
                      (bcf/FUNCV argc)
                      (bcf/FUNCF argc))
                    (ccompile (:body method) (+ 2 slot argtc) (if has-fn-subnode
                                                                parent-freevar-env
                                                                local-env))
                    (bcf/RET (+ 2 argtc slot))])))
    (if (contains? parent-freevar-env :parent)
                              [(bcf/FNEW slot id)
                               (bcf/UCLO 0)]
                              [(bcf/FNEW slot id)])))


;; Discuss how CALL and FNEW work

;; FNEW 4 *some-fn*
;; CALL 4 *args*

;; or

;; FNEW 5 *some-fn*
;; CALL 4 *args*

(defmethod ccompile :local [node slot env]
  (let [source (e/get-env env (str (:name node)))]
    (if (:freevar source)
      [(bcf/GETFREEVAR slot (:freevar source))]
      [(bcf/MOV slot (:slot source))])))

(defmethod ccompile :do [node slot env]
  (let [statements (doall (map ccompile (:statements node) (repeat slot) (repeat env)))
        ret (ccompile (:ret node) slot env)]
    (vec (concat statements ret))))

(defmethod ccompile :maybe-class [node slot env]
  [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:class node))))])

(defmethod ccompile :var [node slot env]
  [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:form node))))])

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

(defmethod ccompile :loop [node slot env]
  (let [bindings (:bindings node)
        binding-slots (drop slot (range))
        first-binding-slot (first binding-slots)
        new-env (apply merge (map (fn [b s] {(str (:name b)) {:slot s}})
                                  bindings
                                  binding-slots))
        loop-id (e/get-id (str (:loop-id node)))
        merge-env (merge env new-env
                         {loop-id
                          {:slot first-binding-slot}})]
    [(map ccompile bindings binding-slots (repeat merge-env))
     (bcf/LOOP {:loop-id loop-id})
     (ccompile (:body node) (+ slot (count bindings)) merge-env)]))

(defmethod ccompile :recur [node slot env]
  (let [exprs (:exprs node)
        exprs-slots (drop slot (range))
        src (first exprs-slots)
        loop-id (e/get-id (str (:loop-id node)))]
    [(map ccompile exprs exprs-slots (repeat env))
     (bcf/BULKMOV (:slot (e/get-env env loop-id)) src (count exprs))
     (bcf/JUMP {:loop-id loop-id})]))

;; ----------------------- file output --------------------------------

 ^:always-validate
(sm/defn gen-bytecode-output-data :- bcv/Bytecode-Output-Data
  [bc :- bcv/Bytecode-List]
    (let [bytecode-output (assoc-in @bcf/constant-table [:CFUNC 0] bc)]
      (bcf/set-empty)
      bytecode-output))


(sm/defn ^:always-validate gen-file-output
  [bc-output :- bcv/Bytecode-Output-Data]
  (let [bytecode-output-json (with-out-str (json/pprint bc-output))]
    (spit "clojure-bc.json" bytecode-output-json)))

;; ----------------------- main compile ----------------------------

(defn print-node [node]
  (p/pprint (dissoc (anal/env-kick node) :meta)))

(defn c0 [node]
  (vec (flatten (ccompile node 0 {}))))

(defn c [clj-form]
  (let [node (anal/ast clj-form)
        bc (c0 node)
        bc-exit (conj bc {:op :EXIT :a 0 :d 0})
        bc-output (gen-bytecode-output-data bc-exit)]
    (println "Visualiser Index: " (let [bc-server-post (v/bc-post bc-output)]
                                    (when bc-server-post
                                      (:index (:body bc-server-post)))))
    (p/pprint bc-output)
    bc-output))

;; ------------------------ LOOP  ----------------------

#_(c '(let [a 8 b 9]
      ((fn [c d] b))))

#_(c '(let [a 1] a))

#_(c '(loop [a 0]
        (recur (+ a 1))))

;; ------------------------ NOT  ----------------------

#_(p/pprint (anal/env-kick (anal/ast '(not true))))

#_(p/pprint (c (anal/ast '(not false))))

;; --------------------------- gen-file-output ---------------------------------

#_(gen-bytecode-output-data (c0 (anal/ast (+ 1 1))))

#_(gen-file-output (c0 (anal/ast (fn [a b] (+ a b)))))

;; --------------------------- any fn ---------------------------------

#_(anal/env-kick (anal/ast (fn [a] a)))

#_(def any-fn-test (anal/ast  '(do (def a 83) (fn [b] (+ b 8)))))

#_(def any-fn-test-2 (anal/ast (do (fn [a] a) (fn [b] (+ b 1)))))

#_(p/pprint (anal/env-kick (anal/ast '(t))))

#_(bcf/set-empty)

#_(p/pprint (c any-fn-test))

#_(p/pprint (c (anal/ast '(do
                          (def t (fn [] 99))
                          (t)))))



#_(p/pprint (anal/env-kick (anal/ast '(fn [b] 1))))




#_(p/pprint (c (anal/ast '(t))))


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


