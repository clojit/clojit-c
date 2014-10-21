(ns clojit.bytecode-fn
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]
    [clojure.string :as str]))

(declare find-constant-index bc-gen)

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(def empty-constant-table
                     {:CSTR []
                      :CKEY []
                      :CINT []
                      :CFLOAT []
                      :CFUNC {}
                      :types {}
                      :protocols {}
                      :uuid-counter 30
                      :uuid-counter-type 0})

(def constant-table (ref empty-constant-table))

(defn bc-gen [inst a-slot b-slot c-slot]
  {:op inst
   :a a-slot
   :b b-slot
   :c c-slot})

(defn ADDVV [a-slot b-slot c-slot]
  (bc-gen :ADDVV a-slot b-slot c-slot))

(defn SUBVV [a-slot b-slot c-slot]
  (bc-gen :SUBVV a-slot b-slot c-slot))

(defn MULVV [a-slot b-slot c-slot]
  (bc-gen :MULVV a-slot b-slot c-slot))

(defn DIVVV [a-slot b-slot c-slot]
  (bc-gen :DIVVV a-slot b-slot c-slot))

(defn MODVV [a-slot b-slot c-slot]
  (bc-gen :MODVV a-slot b-slot c-slot))

(defn POWVV [a-slot b-slot c-slot]
  (bc-gen :POWVV a-slot b-slot c-slot))

(defn ISLT [a-slot b-slot c-slot]
  (bc-gen :ISLT a-slot b-slot c-slot))

(defn ISGE [a-slot b-slot c-slot]
  (bc-gen :ISGE a-slot b-slot c-slot))

(defn ISLE [a-slot b-slot c-slot]
  (bc-gen :ISLE a-slot b-slot c-slot))

(defn ISGT [a-slot b-slot c-slot]
  (bc-gen :ISGT a-slot b-slot c-slot))

(defn ISEQ [a-slot b-slot c-slot]
  (bc-gen :ISEQ a-slot b-slot c-slot))

(defn ISNEQ [a-slot b-slot c-slot]
  (bc-gen :ISNEQ a-slot b-slot c-slot))

(defn JUMPF [a-slot d-slot]
  {:op :JUMPF
   :a a-slot
   :d d-slot})

(defn JUMPT [a-slot d-slot]
  {:op :JUMPT
   :a a-slot
   :d d-slot})

(defn JUMP [d-slot]
  {:op :JUMP
   :a nil
   :d d-slot})

(defn CALL [a-slot lit]
  [{:op :CALL
    :a a-slot
    :d lit}])

(defn RET [a]
  [{:op :RET
    :a a
    :d nil}])

(defn LOOP [loop-id]
  [{:op :LOOP
    :a nil
    :d loop-id}])

(defn BULKMOV [dst src len]
  [{:op :BULKMOV
    :a dst
    :b src
    :c len}])

(defn MOV [a-slot d-slot]
  [{:op :MOV
    :a a-slot
    :d d-slot}])

(defn NOT [a-slot d-slot]
  [{:op :NOT
    :a a-slot
    :d d-slot}])

(defn NEG [a-slot d-slot]
  [{:op :NEG
    :a a-slot
    :d d-slot}])

(defn NSGETS [a-slot d-slot-str]
  [{:op :NSGETS
    :a a-slot
    :d d-slot-str}])

(defn NSSETS [a-slot d-slot]
  [{:op :NSSETS
    :a a-slot
    :d d-slot}])

(defn constant-table-bytecode [bytecode a-slot const]
  [{:op bytecode
    :a a-slot
    :d (find-constant-index bytecode const)
    :const const}])

(defn CSHORT [dst lit]
  [{:op :CSHORT
    :a dst
    :d lit}])

(defn bool-bytecode [a-slot const]
  [{:op :CBOOL
    :a a-slot
    :d (if const 1 0)
    :const const}])

(defn FUNCF ([a-slot-arg-count]
             (FUNCF a-slot-arg-count nil))
            ([a-slot-arg-count id]
             [{:op :FUNCF
               :a a-slot-arg-count
               :d id}]))

(defn FUNCV ([a-slot-arg-count]
             (FUNCV a-slot-arg-count nil))
            ([a-slot-arg-count id]
             [{:op :FUNCV
               :a a-slot-arg-count
               :d id}]))

(defn FNEW [a-slot d-slot]
  [{:op :FNEW
    :a a-slot
    :d d-slot}])

(defn VFNEW [dst protocol-fn]
  [{:op :VFNEW
    :a dst
    :d protocol-fn}])

(defn CNIL [a-slot]
  [{:op :CNIL
    :a a-slot
    :d nil}])

(defn NEWARRAY [dst size]
  [{:op :NEWARRAY
    :a dst
    :d size}])

(defn GETARRAY [dst src idx]
  [{:op :GETARRAY
    :a dst
    :b src
    :c idx}])

(defn SETARRAY [dst src idx]
  [{:op :SETARRAY
    :a dst
    :b src
    :c idx}])

(defn GETFREEVAR [dst idx name]
  [{:op :GETFREEVAR
    :a dst
    :d idx}])

(defn UCLO [slot upval]
  [{:op :UCLO
    :a slot
    :d upval}])

(defn DROP [start-var end-var]
  [{:op :DROP
    :a start-var
    :d end-var}])

(defn TRANC [start-var]
  [{:op :TRANC
    :a nil
    :d start-var}])

(defn ALLOC [dst typ]
  [{:op :ALLOC
    :a dst
    :d typ}])

(defn CTYPE [dst typ]
  [{:op :CTYPE
    :a dst
    :d typ}])

(defn SETFIELD [ref offset var]
  [{:op :SETFIELD
    :a ref
    :b offset
    :c var}])

(defn GETFIELD [dst ref offset]
  [{:op :GETFIELD
    :a dst
    :b ref
    :c offset}])

;; ----------------------- CONSTANT TABLE ----------------------------

(defn find-constant-index [op const]
  (first (remove nil? (map-indexed (fn [a b]
                                     (when (= b const)
                                       a))
                                   (op @constant-table)))))

(defn find-fn-index [k]
  (get (:CFUNC @constant-table) k))

(defn put-const-in-constant-table [op const]
  (if (find-constant-index op const)
    @constant-table
    (dosync
     (alter constant-table assoc op (conj (op @constant-table) const)))))

(defn put-in-function-table [k f]
  (dosync
   (alter constant-table assoc-in [:CFUNC k] f)))

(defn set-empty []
  (dosync (alter constant-table (fn [ct] empty-constant-table))))

(defn put-in-constant-table [k v]
  (dosync
   (alter constant-table assoc k v)))


(defn get-and-inc-uuid-counter!
  ([ct] (get-and-inc-uuid-counter! ct :default))
  ([ct type]
   (dosync
    (cond
     (= type :type) (let [uuid (:uuid-counter-type @ct)]
                      (alter ct update-in [:uuid-counter-type] inc)
                      uuid)
     :default (let [uuid (:uuid-counter @ct)]
                (alter ct update-in [:uuid-counter] inc)
                uuid)))))

(defn log [type msg]
  (println type "   : " msg))

(defn put-as-global-name! [name type ct]
  (dosync
   (let [name (if (keyword? name)
                (apply str (next (seq (str name))))
                name)
         current-name (get (:top-level-name ct) name)]
     (when current-name
       (log "WARNING" "Redefinition of name (" name ") of type " (:type current-name) ".
            Currently not supported in compiler, remove conflict, working not garantied"))
     (alter ct assoc-in [:top-level-name name] {:type type
                                                :name name}))))

(defn clean-protcol-method-data [pmd]
  (-> pmd
      (dissoc :line :column :end-line :end-column :doc :arglists)
      (assoc :protocol-method-nr (get-and-inc-uuid-counter! constant-table)
             #_:arglists #_(first (:arglists pmd)))))

(defn add-protocol [protocol-name protocol-methods]
  (dosync
   (let [protocol-name-str (.getName protocol-name)
         protocol-name-nonqualified (last (str/split protocol-name-str #"\."))
         protocol (into {} (map (fn [[name data]]
                                  (put-as-global-name! name :protocol-method-name constant-table)
                                  {name (clean-protcol-method-data data)})
                                protocol-methods))]
     (put-as-global-name! protocol-name-nonqualified :protocol-name constant-table)
     (alter constant-table
            assoc-in
            [:protocols protocol-name-str]
            (assoc protocol :nr (get-and-inc-uuid-counter! constant-table))))))

(defn get-protocol [name ct]
  (get (:protocols ct) name))

(defn clean-type-data [type-data]
  (-> type-data
      (assoc
        :nr (get-and-inc-uuid-counter! constant-table)
        :class-name (.getName (:class-name type-data)))
      (dissoc :env :children :protocol-callsites :keyword-callsites :methods :constants :doc)))

(defn clean-fields [fields]
  (into {} (map-indexed (fn [i field]
                          (let [name (str (:name field))]
                            (put-as-global-name! name :type-name constant-table)
                            {name
                             (-> field
                                 (assoc  :offset i
                                   :tag  (.getName (get field :tag))
                                   :o-tag (.getName (get field :o-tag))
                                   :mutable (if (:mutable field) true false))
                                 (dissoc :tag :o-tag :atom :env :form :op :local))}))
                        fields)))

(defn clean-protocol [protocol]
  (into {} (map (fn [protocol]
                  (let [name (.getName protocol)]
                    (if (re-find #"IType" name)
                      {}
                      {name
                       (assoc
                         (get-protocol name @constant-table)
                         :name name)})))
                protocol)))

(defn add-type [type-name type-data]
  (dosync
   (let [fields (clean-fields (:fields type-data))]
     (put-as-global-name! (str (:name type-data)) :type-name constant-table)
     (alter constant-table
            assoc-in
            [:types type-name]
            (-> type-data
                clean-type-data
                (assoc
                  :nr (get-and-inc-uuid-counter! constant-table :type)
                  :protocols (clean-protocol (:interfaces type-data))
                  :class-name (.getName (:class-name type-data))
                  :fields fields
                  :size (count (keys fields)))
                (dissoc :env :interfaces :children :protocol-callsites :keyword-callsites :methods :constants :doc))))))
