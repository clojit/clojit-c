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
    [clojure.string :as str]
    [schema.macros :as sm]))

(declare ccompile)

#_(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

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

;; ---------------- static-invoke --- Math ----------------------

(defmulti static-invoke (fn [node slot env] (:method node)))

(defmethod static-invoke 'add [node slot env]
  (neutralbinop bcf/ADDVV 0 node slot env))

(defmethod static-invoke 'multiply [node slot env]
  (neutralbinop bcf/MULVV 1 node slot env))

(defmethod static-invoke 'minus [node slot env]
  (let [args (:args node)]
    (if (= 1 (count args))
      (conj (ccompile (first args) slot env) (bcf/NEG slot slot))
      (binop bcf/SUBVV node slot env))))

(defmethod static-invoke 'divide [node slot env]
  (let [args (:args node)]
    (if (> (count args) 1)
      (binop bcf/DIVVV node slot env)
      (let [one-slot slot
            one-bc (bcf/CSHORT one-slot 1)
            arg (first args)
            arg-slot (inc slot)
            arg-bc (ccompile arg arg-slot env)
            div-bc (bcf/DIVVV slot one-slot arg-slot)]
        (flatten [one-bc arg-bc div-bc])))))

;; ------------------- static-invoke --- Comparisant ----------------------

(defmethod static-invoke 'lt [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISLT slot (first arg-slots) (second arg-slots))]))

(defmethod static-invoke 'lte  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISLE slot (first arg-slots) (second arg-slots))]))

(defmethod static-invoke 'gt [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISGT slot (first arg-slots) (second arg-slots))]))

(defmethod static-invoke 'gte  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISGE slot (first arg-slots) (second arg-slots))]))

(defmethod static-invoke 'equiv  [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/ISEQ slot (first arg-slots) (second arg-slots))]))

(defmethod static-invoke :default [node slot env]
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

;; -------------------------------------------------------------

(defmulti invoke (fn [node slot env] ((comp :var :fn) node)))

(defmethod invoke #'mod [node slot env]
  (let [args (:args node)
        arg-slots (drop slot (range))
        arg-bc (mapcat ccompile args arg-slots (repeat env))]
    [arg-bc
     (bcf/MODVV slot (first arg-slots) (second arg-slots))]))

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
  (let [name (str (:name node))]
    (bcf/put-as-global-name! name :def bcf/constant-table)
    (bcf/put-const-in-constant-table :CSTR name)
    [(ccompile (:init node) slot env)
     (bcf/NSSETS slot (bcf/find-constant-index :CSTR name))]))

(defmethod ccompile :invoke [node slot env]
  [(invoke node slot env)])

(defmethod ccompile :prim-invoke [node slot env]
  [(invoke node slot env)])

(defmethod ccompile :static-call [node slot env]
  [(static-invoke node slot env)])

(defmethod ccompile :protocol-invoke [node slot env]
  (let [args        (:args node)
        base        slot
        arg-count   (count args)
        target-slot (+ 2 base)
        arg-slots   (drop (+ 3 base) (range))
        arg-bc      (mapcat ccompile args arg-slots (repeat env))
        func-slot   (inc base)
        lit         (inc arg-count)]
    [(ccompile  (:protocol-fn node) func-slot env)
     (ccompile (:target node) target-slot env)
     arg-bc
     (bcf/CALL base lit)]))

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


(defmethod ccompile :deftype [node slot env]
  (let [type-name (.getName (:class-name node))
        type-node (dissoc node :op :env :form :closed-overs)]
    (bcf/add-type type-name type-node)
    (doseq [method (mapv ccompile (:methods node) (repeat 2) (repeat env))]
      (dosync
       (alter bcf/constant-table
              assoc-in
              [:types type-name :protocols (:interface method) (keyword (:name method)) :loop-id]
              (:loop-id method)))

      (bcf/put-in-function-table (:loop-id method) (:bc method)))
    []))

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
       (bcf/UCLO 2 (dec slot))]
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
    {:loop-id id
     :full-env full-env
     :fixed-arity argc
     :arg-count argtc
     :variadic? (:variadic? node)
     :fn-id (:fn-id node)
     :bc bc}))

(defn get-type-member-offset [class-name local-sym]
  (-> @bcf/constant-table
      :types
      (get (.getName class-name))
      :fields
      (get (str local-sym))
      :offset))

(defmethod ccompile :method [node slot env]
  (let [params  (:params node)
        argtc (count params)
        args-slots (drop (inc slot) (range))
        argc (:fixed-arity node)
        body-compile-slot (inc (+ slot argtc))
        id (e/get-id (:loop-id node))
        local-env (merge (assoc (e/creat-env params args-slots)
                           id
                           {:slot body-compile-slot}
                           (str (:name (:this node)))
                           {:slot 2})
                         (into {} (map (fn [local-sym]
                                         {(str local-sym) {:type :self-load
                                                           :this 2
                                                           :offset (get-type-member-offset (:this (:env (:this node))) local-sym)}})
                                       (keys (:locals (:env (:this node)))))))
        full-env local-env
        bc (vec (flatten [(bcf/FUNCF argc id)
                          (ccompile (:body node) body-compile-slot full-env)
                          (bcf/RET body-compile-slot)]))]
    {:loop-id id
     :interface (.getName (:interface node))
     :name (:name node)
     :ns (-> node :this :env :ns .getName)
     :full-env full-env
     :fixed-arity argc
     :arg-count argtc
     :bc bc}))


(defmethod ccompile :local [node slot env]
  (let [source (e/get-env env (str (:name node)))]
    (cond
     (:freevar source) [(bcf/GETFREEVAR slot (:freevar source) (str (:name node)))]
     (:this source) [(bcf/GETFIELD slot (:this source) (:offset source))]
     :default [(bcf/MOV slot (:slot source))])))

(defmethod ccompile :maybe-class [node slot env]
  (let [source (e/get-env env (str (:class node))) ]
    (if (:freevar source)
      [(bcf/GETFREEVAR slot (:freevar source) (str (:class node)))]
      [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:class node))))])))

(defn is-macroexpand-of-defprotocol [node]
  (= (-> node
         :raw-forms
         first
         first
         str)
     "defprotocol"))

(defmethod ccompile :do [node slot env]
  (if (is-macroexpand-of-defprotocol node)
    (ccompile (assoc node :op :defprotocol) slot env)
    (let [statements (doall (map ccompile (:statements node) (repeat slot) (repeat env)))
          ret (ccompile (:ret node) slot env)]
      (vec (concat statements ret)))))

(defmethod ccompile :defprotocol [node slot env]
  (let [name (-> node
                 :statements
                 (nth 4)
                 :args
                 last
                 :args
                 first
                 :val
                 :on-interface)
        method-meta (-> node
                        :statements
                        (nth 4)
                        :args
                        last
                        :args
                        (nth 2)
                        :expr
                        :val)
        #_typed-methods #_(-> node
                          :statements
                          second
                          :raw-forms
                          first
                          last)]
    (bcf/add-protocol name method-meta)
    ;; clojure returns symbol of name of the protocol
    []))


(defn find-protocol-method-nr [var-sym]
  (first
   (filter (comp not nil?)
           (map
            (fn [[name data]]
              (:protocol-method-nr (get data (keyword var-sym))))
            (:protocols @bcf/constant-table)))))

(defmethod ccompile :var [node slot env]
  (if (bcf/find-constant-index :CSTR (str (:form node)))
    [(bcf/NSGETS slot (bcf/find-constant-index :CSTR (str (:form node))))]
    [(bcf/VFNEW slot (find-protocol-method-nr (:form node)))]))

(defmethod ccompile :import [node slot env]
  [])

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
     (if (= slot after-binding-slot)
       []
       (bcf/MOV slot after-binding-slot))
     (if (> (inc slot) after-binding-slot)
       []
       [] #_(bcf/DROP (inc slot) after-binding-slot))]))

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
     #_(bcf/DROP (inc slot) after-bindings-slot)
     (bcf/TRANC (inc slot))]))

(defmethod ccompile :recur [node slot env]
  (let [exprs (:exprs node)
        exprs-count (count exprs)
        exprs-slots (drop slot (range))
        src (first exprs-slots)
        loop-id (e/get-id (:loop-id node))
        loop-begin-slot (:slot (e/get-env env loop-id))]
    [(map ccompile exprs exprs-slots (repeat env))
     (bcf/BULKMOV loop-begin-slot src exprs-count)
     #_(bcf/DROP (+ loop-begin-slot exprs-count) (+ src exprs-count))
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

(defn get-type-nr [class-name]
  (:nr (get (:types @bcf/constant-table)
                  (.getName class-name))))

(defmethod ccompile :const [node slot env]
  (let [val (:val node)
        op
        (cond
         (anal/is-int? node) :CINT
         (anal/is-float? node) :CFLOAT
         (= :string (:type node)) :CSTR
         (= :keyword (:type node)) :CKEY
         (= :bool (:type node)) :CBOOL
         (= :nil (:type node)) :CNIL
         (= :class (:type node)) :CTYPE
         :default :CNOTHING)]
    (cond
     (= op :CBOOL) (bcf/bool-bytecode slot val)
     (= op :CNIL)  (bcf/CNIL slot)
     (= op :CINT)  (creat-int-constant-bytecode val slot)
     (= op :CTYPE)  (bcf/CTYPE slot (get-type-nr val))
     (some #{op} [:CFLOAT :CKEY :CSTR :CBOOL]) (do
                                               (bcf/put-const-in-constant-table op val)
                                               (bcf/constant-table-bytecode op slot val))
     :default (println "Could not find const type: " (:type node) " val: " val ))))

(defmethod ccompile :new [node slot env]
  (let [args (:args node)
        obj-slot slot
        arg-slots (drop (inc slot) (range))
        set-bc (mapcat (fn [node slot]
                         [(ccompile node slot env)
                          (bcf/SETFIELD obj-slot (or (:id node) (:arg-id node)) slot)])
                       args
                       arg-slots)]
    [(ccompile (:class node) obj-slot env)
     (bcf/ALLOC obj-slot obj-slot)
     set-bc]))

(defmethod ccompile :instance-call [node slot env]
  (println "instance call")
  #_(p/pprint (:form node)))

;; ---------------------- generate vtable --------------------------


;; Confusing function but simple
;; Loops threw all types, all interfaces, then all methods and finds the func index for that method (param) inside of each type

(defn find-implementation [method-nr ct]
  (into {} (filter (comp not nil?)
                  (map (fn [[type-name type-data]]
                         (first (filter (comp not nil?)
                                        (map (fn [[protocol-name protocol-data]]
                                               (let [func (first (filter (comp not nil?)
                                                                         (map (fn [[method-name method-data]]
                                                                                (when-not (or (= :nr method-name)
                                                                                              (= :name method-name))
                                                                                        (when (= method-nr
                                                                                                 (:protocol-method-nr method-data))
                                                                                          (:func method-data))))
                                                                              protocol-data)))]
                                                 (when func
                                                   {(:nr type-data) func} )))
                                             (:protocols type-data)))))
                       (:types ct)))))

(defn generate-vtable [ct]
  (into {} (mapv (fn [[protocol-name protocol-data]]
                   (into {} (map (fn [[protocol-method-name method-data]]
                                   (when-not (= protocol-method-name
                                                :nr)
                                     (let [method-nr (:protocol-method-nr method-data)]
                                       {method-nr (find-implementation method-nr ct)})))
                                 protocol-data)))
                 (:protocols ct))))


(defn creat-type-vector [ct]
  (let [type-map (into {} (map (fn [t]
                                 {(:nr t)
                                  (-> t
                                      (assoc :fields (vec (vals (:fields t)))))})
                               (vals (:types ct))))]
    (mapv #(or (get type-map %)) (range (count type-map)))))

;; ----------------------- main compile ----------------------------

(defn switch-protocol-key [protocols]
  (into {} (map
            (fn [[k-name v-data]]
              {(:nr v-data) (dissoc v-data :nr)})
            protocols)))

(defn c0 [node]
  (vec (flatten (ccompile node 0 {}))))

(defn c [clj-form]
  (let [node  (anal/asteval clj-form)
        bc (c0 node)
        bc-exit (conj bc {:op :EXIT :a 0 :d nil})
        ct1 (bcf/put-in-function-table "0" bc-exit)

        resolved-bc-list (bcp/resolve-jump-offset (:CFUNC @bcf/constant-table))
        ct2 (bcf/put-in-constant-table :bytecode resolved-bc-list)

        resolved-type-methods (bcp/resolve-type-method @bcf/constant-table)
        ct3 (bcf/put-in-constant-table :types resolved-type-methods)

        vtable (generate-vtable @bcf/constant-table)
        ct4 (bcf/put-in-constant-table :vtable vtable)

        ct5 (bcf/put-in-constant-table :types (creat-type-vector @bcf/constant-table))

        protocols (switch-protocol-key (:protocols @bcf/constant-table))

        ct6 (bcf/put-in-constant-table :protocols protocols)

        clean-bc (bcp/remove-landings (:bytecode @bcf/constant-table))
        ct8 (bcf/put-in-constant-table :bytecode clean-bc)

        ct7 (bcf/put-in-constant-table
             :fn-bc-count
             (into {} (map (fn [[k v]]
                             {k (count v)})
                           (:CFUNC @bcf/constant-table))))

        ct @bcf/constant-table]


    (println "------------------")
    (p/pprint clj-form)
    (println "------------------")

    (println "Visualiser Index: " (let [bc-server-post (v/bc-post @bcf/constant-table)]
                                    (when bc-server-post
                                      (:index (:body bc-server-post)))))
    (println "------------------")
    #_(println "Bytecode without jump resolution")
    #_(by-line-print (unresolved-bytecode @bcf/constant-table))
    (println "Types")
    (println "------------------")
    (apply println (bcprint/print-types @bcf/constant-table))
    (println "Const View")
    (p/pprint (dissoc @bcf/constant-table :fn-bc-count :CFUNC :types :protocols :bytecode :top-level-name :uuid-counter :uuid-counter-type :uuid-counter-type))
    (println "------------------")
    (println "Bytecode with jump resolution")
    (bcprint/by-line-print (bcprint/resolved-bytecode-format @bcf/constant-table))
    (bcf/set-empty)
    ct))

(defn cleanup [bc]
  (let [bc-c1 (dissoc bc :CFUNC :fn-bc-count)
        bc-c2 (update-in bc-c1
                         [:bytecode]
                         (fn [bc-list] (map
                                        #(dissoc % :i :const :jt-nr :fnk)
                                        bc-list)))]
    (dissoc bc-c2 :fn-bc-count :CFUNC :uuid-counter :top-level-name :uuid-counter-type :protocols )))

;; ----------------------- file output --------------------------------

(defn clj-file-name [clj-infile]
  (apply str [(first (butlast (str/split clj-infile #"\."))) ".json"]))

(sm/defn gen-file-output
  [bc-output :- bcv/Bytecode-Output-Data output-file-name]
  (let [bytecode-output-json (with-out-str (json/pprint bc-output))]
    (spit output-file-name bytecode-output-json)))

(defn compiler-entery [clj-infile]
  (let [clj-str (slurp clj-infile)
        clj-form-file (edn/read-string clj-str)
        clj-bc (c clj-form-file)
        clj-clean-bc (cleanup clj-bc)]
    (gen-file-output clj-clean-bc (clj-file-name clj-infile))))

;; ------------------------ protocols ----------------------


