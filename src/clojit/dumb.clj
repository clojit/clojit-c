(ns clojit.dumb
  (:use
    [gloss core io]
    [gloss.core.formats :only (to-char-buffer)]
    [gloss.core.protocols :only (write-bytes read-bytes)]
    [gloss.data.bytes :only (take-bytes drop-bytes dup-bytes take-contiguous-bytes buf->string)])
  (:require
    [clojure.pprint :as p]
    [schema.utils :as su]
    [clojit.bytecode-fn :as bcf]))

(defmacro dbg [x] `(let [x# ~x] (do (println '~x "=") (p/pprint x#)) x#))

(defcodec int32 :int32)
(defcodec string-section {:count         (repeated :byte)
                          :offset-vector (repeated :byte)
                          :string-vector (repeated :byte)})

(defcodec cljbc-format-decode [:int32
                               (repeated {:section-id :int32 :size :int32})
                               (repeated :int32)
                               (repeated :int64)
                               (repeated :float64)
                               {:offsets (repeated :int32) :off (repeated (string :utf-8) :delimiters ["\0"])}
                               {:offsets (repeated :int32) :off (repeated (string :utf-8) :delimiters ["\0"])}
                               (repeated {:protocol :uint32 :type :int32 :fn-jump-offset :int32})
                               (repeated {:type-id :uint16 :size :int32})])

(defcodec cljbc-format [(repeated {:section-id :int32 :size :int32})
                        (repeated :int32)
                        (repeated :int64)
                        (repeated :float64)

                        (repeated :int32)                   ; strings offsets
                        (repeated (string :utf-8))          ; strings off
                        (repeated :int32)                   ; keys offsets
                        (repeated (string :utf-8))          ; keys off

                        (repeated {:protocol :uint32 :type :int32 :fn-jump-offset :int32})

                        (repeated {:type-id :int32 :size :int32})])

(def all-op [:CSTR :CKEY :CINT :CFLOAT :CTYPE :CBOOL :CNIL :CSHORT :SETF
             :NSSET :NSGET
             :ADDVV :SUBVV :MULVV :DIVVV :POWVV :MODVV
             :ISLT :ISGE :ISLE :ISGT :ISEQ :ISNEQ
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

(def op-to-num-map (apply merge (map-indexed (fn [i b] (println b ":" i) {b i}) all-op)))

(defn cljbc-format-by-map [config-map]
  (to-byte-buffer (contiguous (encode cljbc-format [(:section-header config-map)
                                                    (:instr config-map)
                                                    (:ints config-map)
                                                    (:floats config-map)
                                                    (:offsets (:strings config-map))
                                                    (:off (:strings config-map))
                                                    (:offsets (:keys config-map))
                                                    (:off (:keys config-map))
                                                    (:vtable config-map)
                                                    (:types config-map)]))))

#_{:offsets [0 7 15] :off ["global" "global2" "->Bar"]}
#_{:offsets [0 7 15] :off ["global" "global2" "->Bar"]}

#_(decode
    (compile-frame {:offsets (repeated :int32) :off (repeated (string :utf-8 :delimiters ["\0"]))})
    (encode
      (compile-frame {:offsets (repeated :int32) :off (repeated (string :utf-8))})
      {:offsets [0 7 15] :off ["global\0" "global2\0" "->Bar\0"]}))

(defn get-types [table]
  (let [types (sort-by :nr (:types table))
        lst (map #(hash-map :def :uint16
                            :val (first (encode (compile-frame :uint16) (:nr %)))
                            :type-id (:nr %)
                            :size (:size %)) types)]
    {:lst lst :size (* 4 (count lst))}))

(def abc-frame (compile-frame {:b :byte :c :byte :a :byte :op :byte}))
(def ad-frame (compile-frame {:d :int16 :a :byte :op :byte}))

(defn instruction-binary [instr]
  (let [f (fn [k] {k (if (nil? (k instr)) 0 (k instr))})
        e (if (:b instr)
            (let [instr (assoc (apply merge (map f [:op :a :b :c])) :iop (op-to-num-map (:op instr)))]
              (println instr)
              (apply + [(bit-shift-left (:iop instr) 24)
                        (bit-shift-left (:a instr) 16)
                        (bit-shift-left (:c instr) 8)
                        (:b instr)]))
            (let [instr (assoc (apply merge (map f [:op :a :d])) :iop (op-to-num-map (:op instr)))]
              (println instr)
              (apply + [(bit-shift-left (:iop instr) 24)
                        (bit-shift-left (:a instr) 16)
                        (:d instr)])))]
    e))

#_(apply + [(:op t) (* (Math/pow 2 8) (:a t)) (* (Math/pow 2 16) (:d t))])

(defn get-instr [table]
  (let [instrs (sort-by :i (:bytecode table))
        lst (mapv #(hash-map :def :int32
                             :data (instruction-binary %)) instrs)]
    {:lst lst :size (* 4 (count lst))}))

(def int-frame (compile-frame :int64))

(defn get-ints [table]
  (let [ints (:CINT table)
        lst (mapv #(hash-map :def :int64 :val (first (encode int-frame %)) :data %) ints)]
    {:lst lst :size (* 8 (count lst))}))

(def float-frame (compile-frame :float64))

(defn get-floats [table]
  (let [floats (:CFLOAT table)
        lst (mapv #(hash-map :def :float64 :val (first (encode float-frame %)) :data %) floats)]
    {:lst lst :size (* 8 (count lst))}))

(def vtable-frame (compile-frame {:protocol :int32 :type :int32 :fn-jump-offset :int32}))

(defn get-vtable [table]
  (let [vtable (:vtable table)
        lst (vec (mapcat (fn [[p impl]]
                           (map (fn [[t offset]]
                                  {:def  [:int32 :int32 :int32]
                                   :val  (first (encode vtable-frame {:protocol p :type t :fn-jump-offset offset}))
                                   :data {:protocol p :type t :fn-jump-offset offset}}) impl)) vtable))]
    {:lst lst :size (* 12 (count lst))}))

(defn get-str-buffer [table constant-key]
  (let [strings (constant-key table)
        string-list (mapv (fn [s] (let [str-name (str (name s) "\0")]
                                    {:data   str-name
                                     :length (count str-name)
                                     :size   (alength (.getBytes str-name "UTF-8"))
                                     })) strings)
        strings-running-sum (reduce (fn [acc str]
                                      (conj acc (assoc str :sum (+ (:size str)
                                                                   (if (empty? acc)
                                                                     0
                                                                     (:sum (last acc)))))))
                                    []
                                    string-list)]
    {:size               (apply + (map :size strings-running-sum))
     :string-vector      (map :data string-list)
     :offset-vector      (map :sum strings-running-sum)
     :offset-byte-vector (first (encode-all (compile-frame :int32) (map :sum strings-running-sum)))
     :count              (first (encode int32 (count strings-running-sum)))}))

(defn get-strings [table]
  (get-str-buffer table :CSTR))

(defn get-keys [table]
  (get-str-buffer table :CKEY))

(def section-frame (compile-frame {:section-id :int32 :size :int32}))

(defn dumb-buffer []
  (println "dumb buffer")
  (p/pprint @bcf/constant-table)
  (let [table @bcf/constant-table
        types (get-types table)
        types-vec-data (mapv #(dissoc % :val :def) (:lst types))
        instr (get-instr table)
        instr-vec-data (mapv #(:data (dissoc % :val :def)) (:lst instr))
        ints (get-ints table)
        ints-vec-data (mapv #(:data (dissoc % :val :def)) (:lst ints))
        floats (get-floats table)
        floats-vec-data (mapv #(:data (dissoc % :val :def)) (:lst floats))
        vtable (get-vtable table)
        vtable-vec-data (mapv #(:data (dissoc % :val :def)) (:lst vtable))
        strings (get-strings table)
        strings-vec-data {:offsets (:offset-vector strings) :off (:string-vector strings)}
        keyys (get-keys table)
        keyys-vec-data {:offsets (:offset-vector keyys) :off (:string-vector keyys)}]
    (let [sections [instr ints floats strings keyys vtable types]
          section-header (map-indexed (fn [i section]
                                        (let [sectionhead {:section-id i
                                                           :size       (:size section)}]
                                          {:data sectionhead
                                           :val  (first (encode section-frame sectionhead))})) sections)
          section-header-data (mapv #(:data (dissoc % :val)) section-header)]
      (cljbc-format-by-map {:types          types-vec-data
                            :section-header section-header-data
                            :instr          instr-vec-data
                            :ints           ints-vec-data
                            :floats         floats-vec-data
                            :vtable         vtable-vec-data
                            :strings        strings-vec-data
                            :keys           keyys-vec-data}))))
