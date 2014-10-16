(ns clojit.bytecode-patch
  (:require
    [clojit.bytecode-fn :as bcf]
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
            (-> bc-list
                (assoc-in [index :landing] (get-in bc-list [index :d]))
                (assoc-in [index :d] nil)
                ))
          bc-list
          landings))

(defn resolve-jumps [bc-list landings jumps]
  (reduce (fn [bc-list [jump-index target]]
            (if-let [landing-index (get landings target)]
              (assoc-in
               (if (= (get-in bc-list [jump-index :op])
                      :FNEW)
                 (assoc-in bc-list [jump-index :d] landing-index)
                 (assoc-in bc-list [jump-index :d] (- landing-index jump-index)))
               [jump-index :jt-nr]
               (+ jump-index (- landing-index jump-index)))
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

#_(defn get-method-name [protocol-method-nr]
  (let [protocols (:protocol @bcf/constant-table)]
    (map (fn [[name data]]

           (map (fn [method-name data]

                  (when (map? data)
                    (if (= protocol-method-nr
                           (:protocol-method-nr data))
                      (:name data))

                    )

                  ) data)

           ) protocols)


    ))

#_(defn vfn-add-protocol [bc-list]
  (mapv (fn [bc] (if (= :VFNEW (:op bc))
                   (assoc bc :name  (get-method-name (:d bc)))
                   bc)) bc-list))


(defn resolve-jump-offset [fn-map]
  (let [bc-list (vec (apply concat (vals fn-map)))
        landings (get-landings bc-list [:FUNCF :FUNCV :nop :LOOP])
        jumps (get-jumps bc-list [:JUMPT :JUMPF :JUMP :FNEW])]
    (-> bc-list
        fn-add-key
        (remove-landings landings)
        (resolve-jumps landings jumps)
        take-out-nops)))

(defn resolve-interface [interface-data bclist]
  (into {} (map (fn [[key data]]
                  (if (or (= key :nr)
                          (= key :name))
                    {key data}
                    {key (dissoc (assoc data :func
                                   (first (filter (comp not nil?) (map (fn [bc] (when (= (:landing bc) (:loop-id data))
                                                                                  (:i bc)))
                                                                       bclist)))) :loop-id)}))
                interface-data)))


(defn resolve-type-method [ct]
  (into {} (map
            (fn [[type-name type-data]]
              (if (= :counter type-name)
                {type-name type-data}
                {type-name
                 (merge
                  (dissoc type-data :interfaces)
                  {:interfaces
                   (into {}
                         (map (fn [[interface-name interface-data]]
                                {interface-name (resolve-interface interface-data (:bytecode ct))})
                              (:interfaces type-data)))})}))
            (:types ct))))

#_(-> (resolve-type-method {:bytecode [{:i 5 :landing "55232"}
                                     {:i 6 :landing "65353"}]
                          :types {"Person" {:otherstuff 5
                                            :interfaces {"test" {:nr 10 :name "test" :blabla {:loop-id "55232"}}
                                                         "test2" {:nr 11 :name "test2" :blabla2 {:loop-id "65353"}}}}
                                  "Person2" {:otherstuff 5
                                            :interfaces {"test" {:nr 10 :name "test" :blabla {:loop-id "55232"}}
                                                         "test2" {:nr 11 :name "test2" :blabla2 {:loop-id "65353"}}}}}
                          })
    p/pprint)
