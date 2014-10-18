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
                      :protocol {}
                      :uuid-counter 0})

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
    :d idx
    :name name}])

(defn UCLO [slot upval]
  [{:op :UCLO
    :a slot
    :d upval}])

(defn DROP [start-var end-var]
  [{:op :DROP
    :a start-var
    :d end-var}])

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
  (let [protocol (-> pmd
                     (dissoc :line :column :end-line :end-column :doc)
                     (assoc :protocol-method-nr (get-in @constant-table [:uuid-counter])
                            :arglists (first (:arglists pmd))))]
    (alter constant-table update-in [:uuid-counter] inc)
    protocol))

(defn add-protocol [protocol-name protocol-methods]
  (dosync
   (let [protocol-counter (:uuid-counter @constant-table)
         protocol-name (.getName protocol-name)
         protocol-name-nonqualified (last (str/split protocol-name  #"\."))
         protocol (into {} (map (fn [[name data]]
                                  (put-as-global-name! name :protocol-method-name constant-table)
                                  {name (clean-protcol-method-data data)})
                                protocol-methods))]
     (put-as-global-name! protocol-name-nonqualified :protocol-name constant-table)
     (alter constant-table
            assoc-in
            [:protocol protocol-name]
            (assoc protocol :nr protocol-counter))
     (alter constant-table update-in [:uuid-counter] inc))))

(defn get-protocol [name ct]
  (-> ct
      :protocol
      (get name)))

(defn add-type [type-name t]
  (dosync
   (let [type-counter (:uuid-counter  @constant-table)
         interface (into {} (map (fn [interface]
                                   (let [name (.getName interface)]
                                     (if (re-find #"IType" name)
                                       {}
                                       {name
                                        (assoc
                                          (get-protocol name @constant-table)
                                          :name name)})))
                                 (:interfaces t)))
         classname (.getName (:class-name t))
         t (-> t
               (assoc :nr type-counter
                      :interfaces interface
                      :class-name classname)
               (dissoc :env :children :protocol-callsites :keyword-callsites :methods :constants :doc))
         fields (:fields t)
         clean-fields (into {} (map-indexed (fn [i field]
                                              (put-as-global-name! (str (:name field)) :type-name constant-table)
                                              {(str (:name field))
                                               (-> field
                                                   (assoc :offset i
                                                          #_:o-tag #_(.getName (:o-tag field))
                                                          #_:tag   #_(.getName (:tag field))
                                                          )
                                                   (dissoc :atom :env :form :tag :o-tag :op :local :mutable))})
                                            fields))
         t (assoc t :fields clean-fields)]
     (put-as-global-name! (str (:name t)) :type-name constant-table)
     (alter constant-table assoc-in [:types type-name] t)
     (alter constant-table update-in [:uuid-counter] inc))))
