(ns clojit.core
  (:require
   [clojit.compiler :as c]
   [clojit.analyzer :as anal]
   [clojure.tools.reader.edn :as edn]
   [clojure.pprint :as p]
   [clojure.tools.analyzer :as a]))


(defn -main [clj-infile]
    (let [clj-str (slurp clj-infile)
          clj-form (edn/read-string clj-str)
          clj-ast (anal/fast clj-form)
          clj-bc (c/c0 clj-ast)]
    (c/gen-file-output clj-bc)))


