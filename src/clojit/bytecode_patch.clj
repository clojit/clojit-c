(ns clojit.bytecode-patch
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]
    [schema.core :as s]))


(defn get-landings [bc-list landings-ops]
  (into {} (map-indexed (fn [i bc]
                          (if (some #{(:op bc)} landings-ops)
                            {(:d bc) i}
                            {}))
                        bc-list)))

(defn get-jumps [bc-list jump-ops]
  (into {} (map-indexed (fn [i bc]
                          (if (some #{(:op bc)} jump-ops)
                            {i (:d bc)}
                            {}))
                        bc-list)))

(defn remove-landings [bc-list landings]
  (reduce (fn [bc-list [id index]]
            (assoc-in  bc-list [index :d] nil))
          bc-list
          landings))

(defn resolve-jumps [bc-list landings jumps]
  (reduce (fn [bc-list [jump-index target]]
            (if-let [landing-index (get landings target)]
              (-> bc-list
                  (assoc-in [jump-index :jt-nr] (+ jump-index (- landing-index jump-index)))
                  (assoc-in [jump-index :d] (- landing-index jump-index)))
              (assoc-in bc-list [jump-index :jt-nr] (+ jump-index (get-in bc-list [jump-index :d])))))
          bc-list
          jumps))

(defn take-out-nops [bc-list]
  (map-indexed #(assoc %2 :i %1)
               (flatten (mapv
                         #(if (= (:op %) :nop) (:a %) %)
                         bc-list))))


(defn fn-add-key [bc-list]
  (mapv (fn [bc] (if (= :FNEW (:op bc))
                   (assoc bc :fnk (:d bc))
                   bc)) bc-list))


#_(p/pprint (-> [{:op :FNEW :a 0 :d "1234"}{:op :FUNCF :a 0 :d nil}{:op :CALL :a 0 :d 0}]
              fn-add-key))

(defn resolve-jump-offset [fn-map]
  (let [bc-list (vec (apply concat (vals fn-map)))
        landings (get-landings bc-list [:FUNCF :FUNCV :nop :LOOP])
        jumps (get-jumps bc-list [:JUMPT :JUMPF :JUMP :FNEW])]
    (-> bc-list
        fn-add-key
        (remove-landings landings)
        (resolve-jumps landings jumps)
        take-out-nops)))

#_(resolve-jump-offset fnmap)
