(ns clojit.dumb
  (:use
    [gloss core io]
    [gloss.core.formats :only (to-char-buffer)]
    [gloss.core.protocols :only (write-bytes read-bytes)]
    [gloss.data.bytes :only (take-bytes drop-bytes dup-bytes take-contiguous-bytes buf->string)])
  (:require
    [clojure.pprint :as p]
    [schema.utils :as su]
    [clojit.bytecode-fn :as bcf]
    ))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defcodec int32 :int32)
(defcodec string-section {:count (repeated :byte)
                          :offset-vector (repeated :byte)
                          :string-vector (repeated :byte)})

(defcodec cljbc-format-decode [:int32
                               (repeated {:section-id :int32 :size :int32})
                               (repeated :int32)
                               (repeated :int64)
                               (repeated :float64)
                               {:offsets (repeated :int32) :off (repeated  (string :utf-8) :delimiters ["\0"])}
                               {:offsets (repeated :int32) :off (repeated  (string :utf-8) :delimiters ["\0"])}
                               (repeated {:protocol :uint32 :type :int16 :fn-jump-offset :int32})
                               (repeated {:type-id :uint16 :size :int32})])

(defcodec cljbc-format [:int32
                        (repeated {:section-id :int32 :size :int32})
                        (repeated :int32)
                        (repeated :int64)
                        (repeated :float64)
                        {:offsets (repeated :int32) :off (repeated  (string :utf-8))}
                        {:offsets (repeated :int32) :off (repeated  (string :utf-8))}
                        (repeated {:protocol :uint32 :type :int16 :fn-jump-offset :int32})
                        (repeated {:type-id :uint16 :size :int32})])

(def all-op [:CSTR :CKEY :CINT :CFLOAT :CTYPE :CBOOL :CNIL :CSHORT
             :NSSETS :NSGETS
             :ADDVV :SUBVV :MULVV :DIVVV :POWVV
             :ISLT :ISGE :ISLE :ISGT :ISNEQ
             :MOV :NOT :NEG
             :JUMP :JUMPF :JUMPT
             :CALL :RET
             :APPLY
             :FNEW :VFNEW :GETFREEVAR :UCLO
             :LOOP :BULKMOV
             :NEWARRAY :GETARRAY :SETARRAY
             :FUNCF :FUNCV
             :ALLOC
             :SETFIELD :GETFIELD
             :BREAK :EXIT :DROP :TRANC])

(def op-to-num-map (apply merge (map-indexed (fn [i b] {b i}) all-op)))


#_(

   #_{:offsets (repeated :int32) :lst (repeated  (string :utf-8))}
                        #_{:offsets (repeated :int32) #_( :lst (string :utf-8))}
                        #_(repeated {:protocol :uint32 :type :int16 :fn-jump-offset :int32})
                        #_(repeated {:type-id :uint16 :size :int32})
   )

#_(
    [{:offsets [5 2] #_( :lst ["string1" "string2"])}]
                              {:offsets [9 1] #_( :lst ["key1" "key2"])}
   )

#_[{:protocol 4 :uint16 2 :fn-jump-offset 1}]
#_[{:type-id 2 :size 2}]
#_[1 2 3 4 5 6]
#_[5.0 3.0 4.5 4.1 5.7 6.9]


#_(defcodec OpCode (enum {:op :byte :a :byte :b :byte :c :byte} {:op :byte :a :byte :d :int16}))







(defn cljbc-format-by-map [config-map]
  (decode cljbc-format-decode
         (encode cljbc-format [ 5
                               (:section-header  config-map)
                               (:instr config-map)
                               (:ints config-map)
                               (:floats config-map)
                               {:offsets [0 7 15] :off ["global" "global2" "->Bar" ] }
                               {:offsets [0 7 15] :off ["global" "global2" "->Bar"] }
                               #_(:strings config-map)
                               #_(:keys config-map)
                               (:vtable config-map)
                               (:types config-map)])))

(decode
 (compile-frame {:offsets (repeated :int32) :off (repeated  (string :utf-8 :delimiters ["\0"]))})
 (encode
  (compile-frame {:offsets (repeated :int32) :off (repeated  (string :utf-8))})
  {:offsets [0 7 15] :off ["global\0" "global2\0" "->Bar\0" ] }))

(defn get-types [table]
  (let [types (sort-by :nr (:types table))
        lst (map #(hash-map :def :uint16
                            :val (first (encode (compile-frame :uint16) (:nr %)))
                            :type-id (:nr %)
                            :size (:size %) ) types)]
    {:lst lst :size (* 4 (count lst)) } ))

(def abc-frame (compile-frame {:b :byte :c :byte :a :byte :op :byte}))
(def ad-frame  (compile-frame {:d :int16 :a :byte :op :byte }))

(defn instruction-binary [instr]
  (let [f (fn [k] {k (if (nil? (k  instr)) 0 (k instr))})
        e (if (:b instr)
            (let [instr  (assoc (apply merge (map f [:op :a :b :c])) :op (op-to-num-map (:op instr)))]
              (apply + [(bit-shift-left (:op instr) 24)
                        (bit-shift-left (:a instr) 16)
                        (bit-shift-left (:c instr) 8)
                        (:b instr)]))
            (let [instr (assoc (apply merge (map f [:op :a :d]))    :op (op-to-num-map (:op instr)))]
              (apply + [(bit-shift-left (:op instr) 24)
                        (bit-shift-left (:a instr) 16)
                        (:d instr)])))]
    e))

(def t {:op 10 :a 2 :d 5})

#_(apply + [(:op t) (* (Math/pow 2 8) (:a t)) (* (Math/pow 2 16) (:d t))])

(defn get-instr [table]
  (let [instrs (sort-by :i (:bytecode table))
        lst (mapv #(hash-map :def :int32
                             :val  (encode int32 (instruction-binary %))
                             :data (instruction-binary %)   ) instrs)]
    {:lst  lst :size (* 4 (count lst))}))

(def int-frame (compile-frame :int64))

(defn get-ints [table]
  (let [ints (:CINT table)
        lst (mapv #(hash-map :def :int64 :val (first (encode int-frame %)) :data %) ints)]
    {:lst lst :size  (* 8 (count lst))}))

(def float-frame (compile-frame :float64))

(defn get-floats [table]
  (let [floats (:CFLOAT table)
        lst (mapv #(hash-map :def :float64 :val (first (encode float-frame %)) :data %) floats)]
    {:lst lst :size  (* 8 (count lst))}))


(def vtable-frame (compile-frame {:protocol :int32 :type :int32 :fn-jump-offset :int32}))

(defn get-vtable [table]
  (let [vtable (:vtable table)
        lst (vec (mapcat (fn [[p impl]]
                           (map (fn [[t offset]]
                                  {:def [:int32 :int32 :int32]
                                   :val (first (encode vtable-frame {:protocol p :type t :fn-jump-offset offset}))
                                   :data {:protocol p :type t :fn-jump-offset offset}}) impl)) vtable))]
    {:lst lst :size (* 12 (count lst)) }))

(defn get-str-buffer [table constant-key]
  (let [strings (constant-key table)
        string-list (mapv (fn [s] (let [str-name (do  (str (name s) "\0"))
                                        string-frame (compile-frame (string :utf-8 :length (count str-name)))
                                         bytestr (first (encode string-frame str-name))]
                                      {:data s
                                       :length (count str-name)
                                       :size (alength (.getBytes str-name "UTF-8"))
                                       :def [:string :utf8 :length (count str-name)]
                                       :val bytestr})) strings)
        strings-running-sum (reduce (fn [w1 w2]
                                   (conj w1 (assoc w2 :sum (apply + (map :size w1)) )  ))
                                 []
                                 string-list)]


    {:size (apply + (map :size strings-running-sum))
     :string-byte-vector (map :val strings-running-sum)
     :string-vector (map :data strings-running-sum)
     :offset-vector (map :sum strings-running-sum)
     :offset-byte-vector (first (encode-all (compile-frame :int32) (map :sum strings-running-sum)))
     :count (first (encode int32 (count strings-running-sum)))}))


(defn get-strings [table]
  (get-str-buffer table :CSTR))

(defn get-keys [table]
  (get-str-buffer table :CKEY))

(def section-frame (compile-frame {:section-id :int32 :size :int32}))

(defn dumb-buffer []
  (let [table @bcf/constant-table
        types (get-types table)
        types-vec (map :val (:lst types))
        types-vec-data (mapv #(dissoc % :val :def) (:lst types))

        instr (get-instr table)
        instr-vec (map :val (:lst instr))
        instr-vec-data (mapv #(:data (dissoc % :val :def)) (:lst instr))

        ints (get-ints table)
        ints-vec (map :val (:lst ints))
        ints-vec-data (mapv #(:data (dissoc % :val :def)) (:lst ints))

        floats (get-floats table)
        floats-vec (map :val (:lst floats))
        floats-vec-data (mapv #(:data (dissoc % :val :def)) (:lst floats))

        vtable (get-vtable table)
        vtable-vec (map :val (:lst vtable))
        vtable-vec-data (mapv #(:data (dissoc % :val :def)) (:lst vtable))

        strings (get-strings table)
        strings-vec (concat [(:offset-vector strings)] (:string-vector strings))
        strings-vec-data {:offsets (:offset-vector strings) :off (:string-vector  strings)}

        keyys (get-keys table)
        keyys-vec (concat [(:offset-vector keyys)] (:string-vector strings))
        keyys-vec-data (bcf/dbg {:offsets (:offset-vector strings) :off (:string-vector  strings)}) ]
    (let [sections [instr ints floats strings keyys vtable types]
          complet-vec (apply concat [instr-vec ints-vec floats-vec strings-vec vtable-vec types-vec])
          section-header (map-indexed (fn [i section]
                                        (let [sectionhead {:section-id (* 0x100 i)
                                                           :size (:size section)}]
                                          {:data sectionhead
                                           :val (first (encode section-frame sectionhead)) } )) sections)
          section-header-data (mapv #(:data (dissoc % :val)) section-header)
          bytecodemap  (apply concat [(first (encode int32 (count section-header)))] ;:section-header-size
                                     (map :val  section-header)  ; sections-header
                              [complet-vec])]

      (p/pprint (cljbc-format-by-map {:types  types-vec-data
                                      :section-header section-header-data
                                      :instr   instr-vec-data
                                      :ints   ints-vec-data
                                      :floats floats-vec-data
                                      :vtable vtable-vec-data
                                      :strings strings-vec-data
                                      :keys keyys-vec-data})))))

#_(let [ss   (buffy/spec :types (buffy/repeated-type (buffy/composite-type (buffy/string-type 256) (buffy/int32-type))
                                                       (count (:types @bcf/constant-table))))
          buf2 (buffy/compose-buffer ss)]
          (buffy/set-field buf2 :types (mapv (fn [t] [(str (:name t)) (:size t)] ) (:types @bcf/constant-table)))

      #_(clojurewerkz.buffy.util/hex-dump )

          (println (buffy/decompose buf2))
        )
