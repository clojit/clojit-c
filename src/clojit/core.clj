(ns clojit.core
  (:require
   [clojit.compiler :as c]
   [clojure.tools.reader.edn :as edn])
  (:gen-class))


(defn -main [clj-infile]
  (c/compiler-entery clj-infile))
