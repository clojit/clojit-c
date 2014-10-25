(ns clojit.core-test
  (:require [clojure.test :refer :all]
            [clojit.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

#_(-> '(do
       (defprotocol REST
         (GET [self]))
       (deftype API [a]
         REST
         (GET [self] a))
      (GET (->API 5)))
    anal/asteval
    :statements
    p/pprint)

#_(c '(do
       (defprotocol REST
         (GET [self])
         (POST [self a b])
         (PUT [self a b c]))

       (defprotocol REST2
         (PATCH [self]))

       (deftype API [a]
         REST
         (GET  [self] a)
         (POST [self a b] a)
         (PUT  [self a b c] a)
         REST2
         (PATCH [self] 101))

      (deftype API2 [a b]
        REST2
        (PATCH [self] 101))


      (PATCH (->API2 5 5))
       ))



#_(-> '(do

       (defprotocol REST
         (GET [self]))

       (deftype API [a]
         REST
         (GET [self] a))

       (let [b (API 5)]
         (GET b))
       )
    anal/ast
    :ret
    :body
    :ret
    keys
    p/pprint
    )


#_(-> '(do

       (defprotocol REST
         (GET [self]))

       (deftype API [a]
         REST
         (GET [self] a))


       (println (API. 101))
       )
    anal/ast
    :ret
    :fn
    :op)

#_(c '(do
      (defprotocol REST
        (GET [self]))

      (deftype API [a]
        REST
        (GET [self] a))))



#_(-> '(do
       (defprotocol bla
         (bla2 [self b])
         (blabla [self c d]))
       (deftype Pipi [a]
         bla
         (bla2 [self b] b)
         (blabla [self c d]
                 (+ c d a))))
   c
    )


#_(def past (-> '(defprotocol pkill
                 (shot-self [self])
                 (fall-on-sword [self ^long a])
                 (jump-off-cliff [self ^long a ^long b]))
              anal/ast))


#_(c '(do (deftype Person [a])
        (defprotocol pkill
          (shot-self [self]))
      10))

#_(c '(deftype a [b]))

#_(-> '(deftype Person [a]
       pkill
       (shot-self [self]
                  (+ 101 102))
  anal/ast
  :body
  :statements
  first ;;<- deftype
  (dissoc :closed-overs  :fields :protocol-callsites)
  anal/dissoc-env


  p/pprint))

#_(c '(do
      (deftype Person [a b])
      ))

#_(-> '(deftype Person [a]
       #_pkill
       #_(shot-self [self]
                    (println "test print")))
    anal/ast
    :form

    )


#_(deftype Person [a]
  pkill
  (shot-self [self]
             (println "test print")))


#_(-> past
    :form
    p/pprint)


#_(-> past
    :statements
    second
    :raw-forms
    first
    p/pprint)

#_(-> past
    :statements
    second
    :raw-forms
    first
    last
    first
    second
    second
    p/pprint)

#_(-> past
    :form
    p/pprint)

#_(map (fn [a] (if (= (:op a) :invoke)  a)) (-> past :statements))

#_(def past (anal/ast '(defprotocol pkill
                       (shot-self [self]))))

#_(-> past :op)

#_(-> '(deftype* OINT [intprim]) anal/ast :fn)

#_(defprotocol PA
  (foo [self])
  (seta! [self a]))

#_(deftype Bar [a]
  PA
  (foo [self]
       (println "PA foo"))
  (seta! [self a]
       (println "PA seta!")))

#_(def bar_ast '(do
                (deftype bar [foo fuu fii])
                (->bar 1 2 3)))

#_(c bar_ast)

#_(p/pprint (anal/ast bar_ast))

#_(-> '(.a b) anal/ast :op)

#_(-> '(. b seta! 5) anal/ast :op)

#_(-> '(foo b) anal/ast :op)

#_(-> '(.foo b) anal/ast :op)

#_(. b seta! 5)

;; ------------------------ types ----------------------

#_(def iast (anal/ast '(-deftype Ind [i])))


#_(-> iast  anal/env-kick p/pprint)

;; ------------------------ recur  ----------------------


#_(c '(loop [a 1 b 2] (if (== a 5)
                      a
                      (recur a b))))

#_(c '(fn [a b c]
      (let [d 1 e 2 f 3]
        (recur a b c))))

#_(c '(loop [a 1 b 2 c 3] (recur a b c)))

;; ------------------------ UCLO  ----------------------

;; UCLO Problem
#_(c '(fn [x] (fn []
              (let [a 1]
                (fn [] a)))))

#_(c '((fn foo [x]
       (fn bar [y]
         (fn baz []
            x)))
       5))

#_(c '(loop [a 1 b 2]
      (recur a b)))

;; ------------------------ LOOP  ----------------------

#_(c '(let [a 8 b 9]
      ((fn [c d] b))))

#_(c '(let [a 1] a))

#_(c '(loop [a 0]
        (recur (+ a 1))))

#_(c '(fn [e f] (loop [h 1 j 2] (if (== (+ h j) (+ e f))
                                 (- j h)
                                 (recur e f)))))



#_(c '((fn [a] (if a a false)) 100))




#_(c '(do (fn [a b] (let [b 1 c (+ 1 b)]
                    (+ b a b)))
        (fn ([] 99999999999999) ([e f g] (loop [h 1 j 2] (if (== (+ h j) (+ e f g))
                                       (- j h)
                                       (recur e f)))))))

;; ------------------------ NOT  ----------------------

#_(p/pprint (anal/env-kick (anal/ast '(not true))))

#_(p/pprint (c (anal/ast '(not false))))

;; --------------------------- gen-file-output ---------------------------------

#_(gen-file-output (c0 (anal/ast (fn [a b] (+ a b)))))

;; --------------------------- any fn ---------------------------------

#_(anal/env-kick (anal/ast (fn [a] a)))

#_(def any-fn-test (anal/ast  '(do (def a 83) (fn [b] (+ b 8)))))

#_(def any-fn-test-2 (anal/ast (do (fn [a] a) (fn [b] (+ b 1)))))

#_(p/pprint (anal/env-kick (anal/ast '(t))))

#_(bcf/set-empty)

#_(p/pprint (c any-fn-test))

#_(p/pprint (c (anal/ast '(do
                          (def t (fn [] 99))
                          (t)))))



#_(p/pprint (anal/env-kick (anal/ast '(fn [b] 1))))




#_(p/pprint (c (anal/ast '(t))))


;; --------------------------- def ---------------------------------

#_(def def-test (anal/ast (def x 1)))


#_(c def-test)

;; --------------------------- lookup -----------------------------

#_(def invoke-test (anal/ast (my-plus 1 2 3 4)))

#_(c invoke-test)

;; --------------------------- let ---------------------------------

#_(def let-test (anal/ast (let [a 1] (if true (+ a a) (+ 10 a)))))


#_(p/pprint (c let-test))

;; --------------------------- Do -------------------------------------

#_(def do-test (anal/ast (do 1 2)))

;; --------------------------- Function call -------------------------------------

#_(def test-fn-ast (anal/ast (test-fn 1 2)))

#_(p/pprint (anal/env-kick test-fn-ast))

#_(def test-fn-bc (c test-fn-ast))

#_(p/pprint bc-ast-if)

;; --------------------------- ast-if -------------------------------------

#_(def ast-if (anal/ast (if (+ 1 1)
                        (+ 2 2)
                        (+ 3 3))))

#_(def bc-ast-if (flatten (ccompile ast-if 0 env)))

#_(p/pprint (anal/env-kick ast-if))
#_(p/pprint bc-ast-if)


;; --------------------------- bool -------------------------------------

#_(def bool-test (anal/ast (if true
                           (+ 1 6) 9)))

#_(def bc-bool-test (flatten (ccompile bool-test 0 {})))

#_(p/pprint bc-bool-test)

#_(p/pprint (anal/env-kick bc-bool-test))


#_(p/pprint (c (anal/ast (+ 1 1))))
#_(p/pprint @bcf/constant-table)

;; --------------------------- def -------------------------------------

#_(def def-test (anal/ast (def a 1)))

#_(keys def-test)

#_(:name def-test)

#_(:op def-test)


