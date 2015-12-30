(defproject clojit "0.1.0-SNAPSHOT"
  :description "Clojure to Clojure Bytecode Compiler"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.reader "0.8.8"]
                 [org.clojure/data.json "0.2.5"]
                 [org.clojure/tools.trace "0.7.8"]
		             [prismatic/schema "0.2.6"]
                 #_[clj-http "0.9.0"]
                 [org.clojure/tools.analyzer.jvm "0.6.3"]
                 [clojurewerkz/buffy "1.0.1"]
                 [gloss "0.2.5"] ]
  :main clojit.core)
