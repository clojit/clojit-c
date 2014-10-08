(ns clojit.compiler
  (:require
    [clojit.analyzer :as anal]
    [clojit.bytecode-fn :as bcf]
    [clojit.bytecode-validation :as bcv]
    [clojit.visualiser :as v]
    [clojit.env :as e]
    [clojit.bytecode-patch :as bcp]
    [clojit.bytecode-print :as bcprint]
    [clojure.pprint :as p]
    [clojure.data.json :as json]
    [clojure.tools.reader.edn :as edn]
    [clojure.tools.trace :as t]
    [schema.macros :as sm]))

(declare ccompile)


(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

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
      (bcf/put-const-in-constant-table :CINT neutral)
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
            one-bc (bcf/CSHORT one-slot 1)
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

(defmethod invoke #'< [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISLT slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'>=  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISGE slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'>=  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISLE slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'>  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISGT slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'==  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISEQ slot (first arg-slots) (second arg-slots))]))

(defmethod invoke #'aset [node slot env]
  (let [args (:args node)
        arg-slots (take (count args) (drop slot (range)))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (apply bcf/SETARRAY arg-slots)]))

(defmethod invoke :default [node slot env]
  (let [args (:args node)
        base slot
        arg-count (count args)
        arg-slots (drop (+ 2 base) (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))
        func-slot (inc base)
        lit (inc arg-count)]
    [(ccompile (:fn (assoc node :fn-args args)) func-slot env)
     arg-bc
     (bcf/CALL base lit)]))

;; ----------------------- INVOKE -------------------------

(defmulti ccompile (fn [node slot env] (:op node)))

(defmethod ccompile :def [node slot env]
  (bcf/put-const-in-constant-table :CSTR (str (:name node)))
  [(ccompile (:init node) slot env)
   (bcf/NSSETS slot (bcf/find-constant-index :CSTR (str (:name node))))])

(defmethod ccompile :invoke [node slot env]
  [(invoke node slot env)])

(defmethod ccompile :binding [node slot env]
  [(ccompile (:init node) slot env)])

(defn gen-bc-for-arity-selection [jt base fn-id]
  (let [first-free-slot (+ 2 (apply max (keys jt)))]
    (apply concat (map-indexed (fn [i [arity id]]
                                 [(if (= i 0)
                                    {:op :nop :a (bcf/CSHORT first-free-slot arity) :d fn-id}
                                    (bcf/CSHORT first-free-slot arity))
                                  (bcf/ISEQ first-free-slot base first-free-slot)
                                  (bcf/JUMPT first-free-slot id)])
                               jt))))

(defn creat-arity-jumptable [all-fn-bc]
  (into {} (map (fn [{:keys [:arg-count :loop-id]}]
                  {arg-count loop-id})
                all-fn-bc)))

(defn creat-fn-id [node]
  (apply str (interpose "-" (mapv #(str (e/get-id (:loop-id %)))
                                       (:methods node)))))

(defn filter-fn-id [env]
  (into {}
        (map (fn [[k v]]
               (if (re-find #"\A-?\d+" k)
                 {}
                 {k v}))
             env)))

(defmethod ccompile :fn [node slot env]
  (let [fn-id (creat-fn-id node)
        all-fn-bc  (mapv #(ccompile % 2 env) (:methods node))
        arity-count (count all-fn-bc)
        arity-jumptable (creat-arity-jumptable all-fn-bc)
        arity-selection-code (gen-bc-for-arity-selection arity-jumptable slot fn-id)
        filter-env (filter-fn-id (dissoc env :parent))

        generate-uclo (some true? (mapv #(let [full-env (:full-env %)]
                                           (and (contains? full-env :parent)
                                                (not (empty?
                                                      (disj (set (keys (:parent full-env)))
                                                            :parent)))))
                                        all-fn-bc))]
    (bcf/put-in-function-table fn-id
                               (vec (flatten [(if (> arity-count 1)
                                                arity-selection-code
                                                [])
                                              (mapcat :bc all-fn-bc)])))
    (if generate-uclo
      [(bcf/FNEW slot fn-id)
       (bcf/UCLO 0 (dec slot))]
      [(bcf/FNEW slot fn-id)])))


#_(if (and (contains? full-env :parent)  ;; has stuff in it
           (not (empty? (disj (set (keys (:parent full-env))) :parent))))
    [(bcf/FNEW slot fn-id)
     (bcf/UCLO 0 (dec slot))]
    [(bcf/FNEW slot id)])

(defmethod ccompile :fn-method [node slot env]
  (let [params  (:params node)
        argtc (count params)
        args-slots (drop slot (range))
        argc (:fixed-arity node)
        body-compile-slot (+ slot argtc)
        id (e/get-id (:loop-id node))
        local-env (assoc (e/creat-env params args-slots)
                    id
                    {:slot body-compile-slot})
        unused-freevar (e/filter-used-freevars (:body node) local-env (e/get-all-freevars env))
        used-freevar-env (reduce #(dissoc %1 %2) env unused-freevar)
        accessable-env (e/clean-parent-env-from-unaccessables local-env used-freevar-env)
        full-env (e/creat-full-env accessable-env local-env)
        bc (vec (flatten [(if (:variadic? node)
                            (bcf/FUNCV argc id)
                            (bcf/FUNCF argc id))
                          (ccompile (:body node) body-compile-slot full-env)
                          (bcf/RET body-compile-slot)]))]
    (comment
      (dbg env)
      (dbg unused-freevar)
      (dbg used-freevar-env)
      (dbg accessable-env)
      (dbg local-env)
      (dbg full-env))

    {:loop-id id
     :full-env full-env
     :fixed-arity argc
     :arg-count argtc
     :variadic? (:variadic? node)
     :fn-id (:fn-id node)
     :bc bc}))

(defmethod ccompile :local [node slot env]
  (let [source (e/get-env env (str (:name node)))]
    (if (:freevar source)
      [(bcf/GETFREEVAR slot (:freevar source) (str (:name node)))]
      [(bcf/MOV slot (:slot source))])))

(defmethod ccompile :maybe-class [node slot env]
  (let [source (e/get-env env (str (:class node))) ]
    (if (:freevar source)
      [(bcf/GETFREEVAR slot (:freevar source) (str (:class node)))]
      [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:class node))))])))

(defmethod ccompile :do [node slot env]
  (let [statements (doall (map ccompile (:statements node) (repeat slot) (repeat env)))
        ret (ccompile (:ret node) slot env)]
    (vec (concat statements ret))))

(defmethod ccompile :var [node slot env]
  (println "VAR")
  [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:form node))))])

;;-------------------------------------
;; 1..5   slot = test-bc             --
;; 6      jumpf slot (count then-bc) --
;; 7..10  slot = then-bc             --
;; 11     jump (count else- bc)      --
;; 12..15 slot = else-bc             --
;;-------------------------------------

(defmethod ccompile :if [node slot env]
  (let [test-bc  (vec (flatten (ccompile (:test node) slot env)))
        then-bc  (vec (flatten (ccompile (:then node) slot env)))
        else-bc  (vec (flatten (ccompile (:else node) slot env)))]
    [test-bc
     (bcf/JUMPF slot  (+ 2 (count then-bc)))
     then-bc
     (bcf/JUMP (inc (count else-bc)))
     else-bc]))

(defmethod ccompile :let [node slot env]
  (let [bindings (:bindings node)
        binding-slots (drop slot (range))
        new-env (e/creat-env bindings binding-slots)
        merge-env (merge env new-env)
        after-binding-slot (+ slot (count bindings))]
    [(map ccompile bindings binding-slots (repeat merge-env))
     (ccompile (:body node) after-binding-slot merge-env)
     (bcf/MOV slot after-binding-slot)
     (bcf/DROP (inc slot) after-binding-slot)]))

(defmethod ccompile :loop [node slot env]
  (let [bindings (:bindings node)
        binding-slots (drop slot (range))
        first-binding-slot (first binding-slots)
        new-env (e/creat-env bindings binding-slots)
        loop-id  (e/get-id (:loop-id node))
        merge-env  (merge env new-env
                          {loop-id
                           {:slot first-binding-slot}})
        after-bindings-slot  (+ slot (count bindings))]
    [(mapv ccompile bindings binding-slots (repeat merge-env))
     (bcf/LOOP loop-id)
     (ccompile (:body node) after-bindings-slot merge-env)
     (bcf/MOV slot after-bindings-slot)
     (bcf/DROP (inc slot) after-bindings-slot)]))

(defmethod ccompile :recur [node slot env]
  (let [exprs (:exprs node)
        exprs-count (count exprs)
        exprs-slots (drop slot (range))
        src (first exprs-slots)
        loop-id (e/get-id (:loop-id node))
        loop-begin-slot (:slot (e/get-env env loop-id))]
    [(map ccompile exprs exprs-slots (repeat env))
     (bcf/BULKMOV loop-begin-slot src exprs-count)
     (bcf/DROP (+ loop-begin-slot exprs-count) (+ src exprs-count))
     (bcf/JUMP loop-id)]))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn creat-int-constant-bytecode [val slot]
  (let [max-val (exp 2 16)
        min-val 0]
    (if (and (< val max-val) (>= val min-val))
      (bcf/CSHORT slot val)
      (do
        (bcf/put-const-in-constant-table :CINT val)
        (bcf/constant-table-bytecode :CINT slot val)))))

(defmethod ccompile :const [node slot env]
  (let [val (:val node)
        op
        (cond
         (anal/is-int? node) :CINT
         (anal/is-float? node) :CFLOAT
         (= :string (:type node)) :CSTR
         (= :keyword (:type node)) :CKEY
         (= :bool (:type node)) :CBOOL
         (= :nil (:type node)) :CNIL)]
    (cond
     (= op :CBOOL) (bcf/bool-bytecode slot val)
     (= op :CNIL)  (bcf/CNIL slot)
     (= op :CINT)  (creat-int-constant-bytecode val slot)
     :default (do
                (bcf/put-const-in-constant-table op val)
                (bcf/constant-table-bytecode op slot val)))))

;; ----------------------- main compile ----------------------------

(defn c0 [node]
  (vec (flatten (ccompile node 0 {}))))

(defn c [clj-form]
  (let [node (anal/ast clj-form)
        bc (c0 node)
        bc-exit (conj bc {:op :EXIT :a 0 :d nil})
        constant-table (bcf/put-in-function-table "0" bc-exit)
        resolved-bc-list (bcp/resolve-jump-offset (:CFUNC constant-table))
        constant-table (bcf/put-in-constant-table :bytecode resolved-bc-list)
        constant-table (bcf/put-in-constant-table
                        :fn-bc-count
                        (into {} (map (fn [[k v]]
                                        {k (count v)})
                                      (:CFUNC constant-table))))]

    (println "Visualiser Index: " (let [bc-server-post (v/bc-post constant-table)]
                                    (when bc-server-post
                                      (:index (:body bc-server-post)))))

    #_(println "Bytecode without jump resolution")
    #_(by-line-print (unresolved-bytecode constant-table))
    (println "Bytecode with jump resolution")
    (bcprint/by-line-print (bcprint/resolved-bytecode-format constant-table))

    #_(println "Constant Table View")
    (p/pprint (dissoc constant-table :fn-bc-count ))

    (bcf/set-empty)
    constant-table))



(defn cleanup [bc]
  (let [bc-c1 (dissoc bc :CFUNC :fn-bc-count)
        bc-c2 (update-in bc-c1 [:bytecode] (fn [bc-list] (map
                                                          #(dissoc % :i :const :jt-nr :fnk)
                                                          bc-list)))]
    (dissoc bc-c2 :fn-bc-count :CFUNC)))

;; ----------------------- file output --------------------------------

(sm/defn gen-file-output
  [bc-output :- bcv/Bytecode-Output-Data]
  (let [bytecode-output-json (with-out-str (json/pprint bc-output))]
    (spit "clojure-bc.json" bytecode-output-json)))

;; ------------------------ protocols ----------------------


#_(defprotocol pkill
  (shot-self [self]))

#_(def past (anal/ast '(defprotocol pkill
                       (shot-self [self]))))

#_(-> past :op)

;; ------------------------ types ----------------------

#_(def iast (anal/ast '(-deftype Ind [i])))


#_(-> iast  anal/env-kick p/pprint)

;; ------------------------ recur  ----------------------


#_(c '(loop [a 1 b 2] (if (== a 5) a (recur a b))))

#_(c '(fn [a b c]
      (let [d 1 e 2 f 3]
        (recur a b c))))

#_(c '(loop [a 1 b 2 c 3] (recur a b c)))

;; ------------------------ UCLO  ----------------------

;; UCLO Problem
#_(c '(fn [x] (fn []
              (let [a 1]
                (fn [] a)))))

#_(c '((fn foo [x]
       (fn bar [y]
         (fn baz []
            x)))
       5))

(c '(loop [a 1 b 2]
      (recur a b)))


;; ------------------------ LOOP  ----------------------

#_(c '(let [a 8 b 9]
      ((fn [c d] b))))

#_(c '(let [a 1] a))

#_(c '(loop [a 0]
        (recur (+ a 1))))

#_(c '(fn [e f] (loop [h 1 j 2] (if (== (+ h j) (+ e f))
                                 (- j h)
                                 (recur e f)))))


#_(c '(do (fn [a b] (let [b 1 c (+ 1 b)]
                    (+ b a b)))
        (fn ([] 99999999999999) ([e f g] (loop [h 1 j 2] (if (== (+ h j) (+ e f g))
                                       (- j h)
                                       (recur e f)))))))

;; ------------------------ NOT  ----------------------

#_(p/pprint (anal/env-kick (anal/ast '(not true))))

#_(p/pprint (c (anal/ast '(not false))))

;; --------------------------- gen-file-output ---------------------------------

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


#_(c def-test)

;; --------------------------- lookup -----------------------------

#_(def invoke-test (anal/ast (my-plus 1 2 3 4)))

#_(c invoke-test)

;; --------------------------- let ---------------------------------

#_(def let-test (anal/ast (let [a 1] (if true (+ a a) (+ 10 a)))))


#_(p/pprint (c let-test))

;; --------------------------- Do -------------------------------------

#_(def do-test (anal/ast (do 1 2)))

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

#_(p/pprint (anal/env-kick bc-bool-test))


#_(p/pprint (c (anal/ast (+ 1 1))))
#_(p/pprint @bcf/constant-table)

;; --------------------------- def -------------------------------------

#_(def def-test (anal/ast (def a 1)))

#_(keys def-test)

#_(:name def-test)

#_(:op def-test)


