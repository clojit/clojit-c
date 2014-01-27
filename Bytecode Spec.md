Clojure Bytecode Spec
======================


Resources
---------

###LuaJit:
http://wiki.luajit.org/Bytecode-2.0
http://wiki.luajit.org/Optimizations

###Java:
http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.2
http://en.wikipedia.org/wiki/Java_bytecode

###Clojure:
http://clojure.org/special_forms
http://clojure.org/datatypes
http://clojuredocs.org/clojure_core/clojure.core/deftype

https://github.com/clojure/clojurescript/blob/master/src/clj/cljs/compiler.clj
https://github.com/clojure/clojurescript/blob/master/src/clj/cljs/core.clj

https://github.com/clojure/tools.reader
https://github.com/clojure/tools.analyzer

https://github.com/halgari/clojure-py

http://clojure-py.blogspot.de/

###Guile

With Guile 2.2 they will have a VM with Register Bytecode, see:
http://wingolog.org/archives/2013/11/26/a-register-vm-for-guile


Problems:
----------

Clojure was designed to run on a host. Both JVM Clojure and ClojureScript (JS) assume some functionality to be present on the host platform.

These features either have to be inplmented in Clojure ontop of the current special froms or we have to provide native inpmnetation (clojure-py).

Ideas:
---------

There is only one looping construct in clojure, yet we often have more infomration from standard macros (dotimes ...)
. Maybe we can somehow use these information and give to the VM. We also have information about the length of loops over collection (map inc [1 2 3]). We have constant access to the size of persistent collection, this information might help the VM.



Desings:
--------

