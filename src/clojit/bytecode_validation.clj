(ns clojit.bytecode-validation
  (:require
    [clojure.pprint :as p]
    [clojure.tools.trace :as t]
    [schema.core :as s]
    ))


(def Bytecode-Simple
  "A schema that maches all bytecodes"
  {(s/required-key :op) s/Keyword
   (s/optional-key :a) s/Int
   (s/optional-key :b) s/Int
   (s/optional-key :c) s/Int
   (s/optional-key :d) s/Int})


(def Bytecode-abcd
   "A schema for validation of the bytecodes"
  {(s/required-key :op) s/Keyword
   (s/required-key :a) (s/maybe s/Int)})

(def Bytecode-abc
  "A schema for validation of the bytecode B	C	A	OP"
  (merge Bytecode-abcd
         {(s/required-key :b) (s/maybe s/Int)
          (s/required-key :c) (s/maybe s/Int)}))

(def Bytecode-ad
  "A schema for validation of the bytecode D	  A	OP"
  (merge Bytecode-abcd
         {(s/required-key :d) (s/maybe s/Int)}))

(def Bytecode
  "A schema that maches all bytecodes"
  (s/either Bytecode-ad Bytecode-abc))

(def Bytecode-List
  "A schema that maches a seq of Bytecodes"
  [Bytecode])

(def Bytecode-Output-Data
  "A schema for validation of the final bytecode datastructure"
  {:CSTR [(s/maybe s/Str)]
   :CKEY [(s/maybe s/Keyword)]
   :CINT [(s/maybe s/Int)]
   :CFLOAT [(s/maybe double)]
   :CFUNC {s/Int Bytecode-List}
   })

(s/validate Bytecode-List [{:op :NSGETS, :a 2, :d nil} {:op :NSGETS, :a 1, :d nil} {:op :CALL, :a 1, :d 1}]

)