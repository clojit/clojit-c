(ns clojit.bytecode-print
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]))



(defn by-line-print [lst]
  (doseq [s lst]
      (println s)))


(defn resolved-bytecode-split [constant-table]
  (:bcs (reduce (fn [bclist [k c]]

                  {:bc (drop c (:bc bclist))
                   :bcs (flatten [(str "fn: " k)
                                  (take c (:bc bclist))
                                  (:bcs bclist)])})
                {:bc (:bytecode constant-table)
                 :bcs []}
                (:fn-bc-count constant-table))))

(defmulti format-bc (fn [bc] (:op bc)))

(defmethod format-bc :FNEW [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ==> %03d %7s" (:i bc) (:op bc) (:a bc) (:d bc) (:jt-nr bc) (:fnk bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :VFNEW [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ==> %15s" (:i bc) (:op bc) (:a bc) (:d bc) (:name bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :GETFREEVAR [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ; %s" (:i bc) (:op bc) (:a bc) (:d bc) (:name bc))
   #"null"
   (format "%4s" "")))


(defmethod format-bc :JUMP [bc]
  (clojure.string/replace
   (if (:jt-nr bc)
     (format "%03d %11s  %4s         %6s ==> %03d" (:i bc) (:op bc) (:a bc) (:d bc) (:jt-nr bc))
     (format "%03d %11s  %4s         %6s" (:i bc) (:op bc) (:a bc) (:d bc)))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :JUMPF [bc]
  (clojure.string/replace
   (if (:jt-nr bc)
     (format "%03d %11s  %4s         %6s ==> %03d" (:i bc) (:op bc) (:a bc) (:d bc) (:jt-nr bc))
     (format "%03d %11s  %4s         %6s" (:i bc) (:op bc) (:a bc) (:d bc)))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :JUMPT [bc]
  (clojure.string/replace
   (if (:jt-nr bc)
     (format "%03d %11s  %4s         %6s ==> %03d" (:i bc) (:op bc) (:a bc) (:d bc) (:jt-nr bc))
     (format "%03d %11s  %4s         %6s" (:i bc) (:op bc) (:a bc) (:d bc)))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :CINT [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ; %s" (:i bc) (:op bc) (:a bc) (:d bc) (:const bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :CFLOAT [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ; %s" (:i bc) (:op bc) (:a bc) (:d bc) (:const bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :CKEY [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ; %s" (:i bc) (:op bc) (:a bc) (:d bc) (:const bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :CSTR [bc]
  (clojure.string/replace
   (format "%03d %11s  %4s         %6s ; %s" (:i bc) (:op bc) (:a bc) (:d bc) (:const bc))
   #"null"
   (format "%4s" "")))

(defmethod format-bc :default [bc]
  (clojure.string/replace
   (if (:b bc)
     (format "%03d %11s  %4s   %4s  %6s"  (:i bc) (:op bc) (:a bc) (:b bc) (:c bc))
     (format "%03d %11s  %4s         %6s" (:i bc) (:op bc) (:a bc) (:d bc)))
   #"null"
   (format "%4s" "")))

(defn resolved-bytecode-format [constant-table]
  (flatten (map  #(if (map? %)
                    (format-bc %)
                    [%
                     (str "-            op     a      b     c/d")])
                 (resolved-bytecode-split constant-table))))

(defn bytecode-format [bc-list]
  (concat
   [(str "-            op     a      b     c/d")]
   (map-indexed (fn [i bc]
                  (format-bc bc))
                bc-list)))


(defn unresolved-bytecode [constant-table]
  (flatten (map (fn [[k v]]
                  [(str "fn: " k)
                   (bytecode-format v)])
                (:CFUNC constant-table))))


(defn print-types [ct]
  (map-indexed (fn [i t]
         (str "Name: " (:name t) " Nr: " i
              "\nProtocols:\n " (apply str (interpose "\n " (keys (:protocols t)))) "\n------------------\n"))
       (:types ct)))

