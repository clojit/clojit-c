(ns clojit.env
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]
    [clojure.set :as hashset]
    [schema.core :as s]
    [schema.macros :as sm]
    [clojit.analyzer :as anal]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))


(def env
  "A schema for validation of the env"
  {(s/maybe s/Str) {(s/required-key :slot) s/Int
                    (s/optional-key :freevar) s/Int}
   (s/optional-key :parent) (s/recursive #'env)})

(def local-env
  (dissoc env :parent))

(def freevar-env
  {:freevar s/Int})

(def slot-env
  {:slot s/Int})

(def get-env-schema
  (s/either freevar-env slot-env))

(sm/defn ^:always-validate get-id :- s/Str
  [loop-id :- s/Symbol]
  (second (.split #"_" (str loop-id))))

(sm/defn ^:always-validate creat-full-env :- env
  [normal-env :- env local-env :- local-env]

  (let [parent-free-env  (dissoc normal-env :parent)
        parent-env  (:parent normal-env)
        new-freevar-nr  (inc (apply max
                                    (conj (mapv (fn [a]
                                                    (if (number? (:freevar a))
                                                      (:freevar a)
                                                      (- 1)))
                                                  (vals parent-env))
                                          (- 1))))
        freevar-env  (into {} (map (fn [[var-name slot] i]
                                          (println "new freevar " i)
                                          [var-name (assoc slot :freevar i)])
                                        parent-free-env
                                        (drop new-freevar-nr (range))))]


    (merge local-env
           (cond
            (and (empty? freevar-env) (nil? parent-env))  {}
            (empty? freevar-env) {:parent {:parent parent-env}}
            (nil? parent-env)    {:parent freevar-env}
            :default {:parent (assoc freevar-env :parent parent-env)} ))))

(defn has-fn-subnode? [node]
  (cond
   (map? node) (if (= (:op node) :fn)
                 true
                 (some true? (map (fn [[k v]] (if (or (map? v) (vector? v))
                                                      (has-fn-subnode? v))) node)))
   (vector? node) (some true? (mapv has-fn-subnode? node))
   :default false))

(defmulti filter-used-freevars (fn [node env search-bindings] (let [op (:op node)]
                                                  (if (vector? node)
                                                    :vector
                                                    op))))

(defmethod filter-used-freevars :fn [node env search-bindings]
  (let [method  (first (:methods node))
        fn-arg-locals (into #{} (map (comp str :name) (:params method)))
        left-over-search-bindings (hashset/difference  search-bindings  fn-arg-locals)]
    (hashset/union
      (hashset/intersection fn-arg-locals search-bindings)
      (filter-used-freevars (:body method) local-env  left-over-search-bindings))))

(defmethod filter-used-freevars :let [node env search-bindings]
  (let [let-locals (into #{} (map (comp str :name) (:bindings node)))
        left-over-search-bindings (hashset/difference  search-bindings let-locals)]
    (hashset/union
     (hashset/intersection let-locals search-bindings)
     (filter-used-freevars (:body node) local-env  left-over-search-bindings))))

(defmethod filter-used-freevars :local [node env search-bindings]
  #_(println "local")
  (let [name (str (:name node))]
     (if (get env name)
            search-bindings
            (disj search-bindings name))))

(defmethod filter-used-freevars :maybe-class [node env search-bindings]
  #_(println "maybe-class")
  (let [name (str (:class node))]
    (if (get env name)
      search-bindings
      (disj search-bindings name))))

(defmethod filter-used-freevars :loop [node env search-bindings]
  (filter-used-freevars (assoc node :op :let) env search-bindings))

(defmethod filter-used-freevars :vector [node env search-bindings]
  (if (empty? node)
      search-bindings
      (apply hashset/intersection (map (fn [subnode]
                                         (filter-used-freevars subnode env search-bindings)) node))))

(defmethod filter-used-freevars :default [node env search-bindings]
  (let [children (:children node)
        bindings-by-children (map (fn [child]
                                    (filter-used-freevars (get node child) env search-bindings)) children)]
    (if (zero? (count bindings-by-children))
      search-bindings
      (apply hashset/intersection bindings-by-children))))

(defn get-all-freevars [env]
  (disj (hashset/union (set (keys env))
                        (if (contains? env :parent)
                          (get-all-freevars (:parent env))
                          #{:parent})) :parent))

(sm/defn ^:always-validate clean-parent-env-from-unaccessables :- env [local-env :- env old-env :- env]
  (let [local-env-keys (set (keys local-env))
        old-env-keys (set (keys old-env))
        remove-keys-old-env (hashset/intersection local-env-keys old-env-keys)]
    (reduce (fn [env k] (dissoc env k)) old-env remove-keys-old-env)))


(sm/defn get-env :- slot-env
  ([env :- env name :- s/Str]
   (get-env env name false))
  ([env :- env name :- s/Str parent :- s/Bool]
   (if-let [local (get env name)]
     (if parent
       (dissoc local :slot)
       (dissoc local :freevar))
     (when (:parent env)
       (get-env (:parent env) name true)))))

(defn creat-env [bindings slots]
  (into {} (map (fn [i binding]
                  [(str (:name binding)) {:slot i}])
                slots
                bindings)))


#_(p/pprint (creat-full-env {:parent {"x" {:freevar 0, :slot 2}}} {}))

#_(p/pprint (creat-full-env {:parent {"x" {:freevar 0, :slot 2}} "y" {:slot 3}}  {}))

#_(p/pprint (convert-to-freevar {} {}))

#_(p/pprint (convert-to-freevar {} {"d" {:slot 6}}))

#_(p/pprint (convert-to-freevar {"c" {:slot 5} :parent {"b" {:freevar 2 :slot 2} "a" {:freevar 3 :slot 1}}}))



#_(get-all-freevars {:parent {"x" {:freevar 0, :slot 2}}, "y" {:slot 5}})

#_(filter-used-freevars (anal/ast '(do a (let [c 1 b 2] c) (fn [c] c b))) {} #{"a" "b" "c"})

#_(filter-used-freevars (anal/ast '(loop [a 1] (if (= a 5) a (recur (inc a))))) {} #{"a" "b"})

#_(filter-used-freevars (anal/ast '(do 1 1 1 1)) {} #{"a"})


#_(get-env {} "a")
#_(get-env {"a" {:slot 5}} "a")
#_(get-env {"a" {:slot 5} :parent {"c" {:freevar 6 :slot 4}}} "c")

