(defproject clojit "0.1.0-SNAPSHOT"
  :description "Clojure to Clojure Bytecode Compiler"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
		 	           [org.clojure/tools.analyzer "0.1.0-alpha2"]
                 [org.clojure/tools.reader "0.8.3"]
                 [org.clojure/data.json "0.2.4"]
                 [org.clojure/tools.trace "0.7.8"]
		             [prismatic/schema "0.2.6"]
                 ]
  :main clojit.core)
