(ns clojit.bytecode-fn
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]))

(declare find-constant-index bc-gen)

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))

(def empty-constant-table
                     {:CSTR []
                      :CKEY []
                      :CINT []
                      :CFLOAT []
                      :CFUNC {}
                      :types {:counter 0}})


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

(defn add-type [type-name t]
  (dosync
   (let [type-counter (:counter (:types @constant-table))
         interface (mapv #(.getName %) (:interfaces t))
         classname (.getName (:class-name t))
         t (-> t
               (assoc :nr type-counter :interfaces interface :class-name classname)
               (dissoc :env ))
         fields (:fields t)
         clean-fields (map (fn [field]
                             (-> field
                                 (assoc :o-tag (.getName (:o-tag field)) :tag (.getName (:tag field)))
                                 (dissoc :atom :env :form)))
                           fields)
         t (assoc t :fields clean-fields)]
     (alter constant-table assoc-in [:types type-name]  t)
     (alter constant-table update-in [:types :counter] inc))))


