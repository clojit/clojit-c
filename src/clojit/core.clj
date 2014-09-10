(ns clojit.core
  (:require
   [clojit.compiler :as c]
   [clojit.analyzer :as anal]
   [clojure.tools.reader.edn :as edn]
   [clojure.pprint :as p]
   [clojure.tools.analyzer :as a]))



(defn read-from-file-with-trusted-contents [filename]
  (with-open [r (java.io.PushbackReader.
                 (clojure.java.io/reader filename))]
    (binding [*read-eval* false]
      (edn/read r))))

(defn -main2 [clj-file]

  (let [clj-form (read-from-file-with-trusted-contents clj-file)
        clj-ast (anal/ast clj-form)
        bc (c/c0 clj-ast)]
    (p/pprint clj-form)
    (p/pprint clj-ast)
    (p/pprint bc)
    (c/gen-bytecode-output-data bc)
    )

  #_(c/gen-file-output (c/c0 (anal/ast (read-from-file-with-trusted-contents clj-file))))
  )


(defn -main [clj-str]
    (let [clj-form (edn/read-string clj-str)
          clj-ast (anal/fast clj-form)
          clj-bc (c/c0 clj-ast)]
    (c/gen-file-output clj-bc)))


