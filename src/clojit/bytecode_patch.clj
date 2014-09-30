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
            (assoc-in bc-list [index :d] nil))
          bc-list
          landings))

(defn resolve-jumps [bc-list landings jumps]
  (reduce (fn [bc-list [jump-index target]]
            (if-let [landing-index (get landings target)]
              (assoc-in bc-list
                        [jump-index :d]
                        (- landing-index jump-index))
              bc-list))
          bc-list
          jumps))

(defn take-out-nops [bc-list]
  (mapv  (fn [bc] (if (= (:op bc) :nop)
                   (:a bc)
                   bc))  bc-list))

(defn resolve-jump-offset [fn-map]
  (let [bc-list (vec (apply concat (vals fn-map)))
        landings (get-landings bc-list [:FUNCF :FUNCV :nop :LOOP])
        jumps (get-jumps bc-list [:JUMPT :JUMPF :JUMP :FNEW])]
    (-> bc-list
        (remove-landings landings)
        (resolve-jumps landings jumps)
        (take-out-nops))))

