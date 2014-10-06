(ns clojit.core
  (:require
   [clojit.compiler :as c]
   [clojit.analyzer :as anal]
   [clojure.tools.reader.edn :as edn]
   [clojure.pprint :as p]
   [clojure.tools.analyzer :as a])
  (:gen-class))


(defn -main [clj-infile]
    (let [clj-str (slurp clj-infile)
          clj-form (edn/read-string clj-str)
          clj-bc (c/c clj-form)
          clj-clean-bc (c/cleanup clj-bc)
          ]
    (c/gen-file-output clj-clean-bc)))


