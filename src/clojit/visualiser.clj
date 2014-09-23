(ns clojit.visualiser
  (:require
    [clojure.pprint :as p]
    [clj-http.client :as client]
    [clojure.data.json :as json]
    [schema.utils :as su]))


(def bc2
{:CSTR ["t" "f"],
 :CKEY [:test :ckey :my-keytest],
 :CINT [99 1 2 3],
 :CFLOAT [1.6 9.0 6.0 1.6 9.0]
 :CFUNC
 {0
  [{:op :CFUNC, :a 0, :d 6001}
   {:op :NSSETS, :a 0, :d 0}
   {:op :CFUNC, :a 0, :d 6002}
   {:op :NSGETS, :a 2, :d 0}
   {:op :CALL, :a 2, :d 0}
   {:op :ADDVV, :a 0, :b 0, :c 1}],
  6001
  [{:op :FUNCV, :a 0, :d nil}
   {:op :CINT, :a 1, :d 0}
   {:op :CINT, :a 3, :d 1}
   {:op :MOV, :a 4, :d 0}]
  6002
  [{:op :FUNCF, :a 3, :d nil}
   {:op :CINT, :a 3, :d 1}
   {:op :MOV, :a 4, :d 0}
   {:op :DIVVV, :a 3, :b 3, :c 4}
   {:op :RET, :a 3, :d nil}],
  6003
  [{:op :FUNCV, :a 0, :d nil}
   {:op :CINT, :a 1, :d 0}
   {:op :RET, :a 1, :d nil}]}})


(def +port+
  "A random port on which to host the service"
  9090)

(def +base-url+
  (format "http://localhost:%s/overseer/" +port+))

(defn http-post [url body]
  (client/post
   (str +base-url+ url)
   {:content-type :json
    :as :json
    :throw-exceptions false
    :body body}))

(defn http-get [url & [qps]]
  (client/get
   (str +base-url+ url)
   (su/assoc-when
    {:as :json
     :throw-exceptions false}
    :query-params qps)))

(defn bc-post [complet-bc]
  (http-post "bcinit"
             (json/write-str complet-bc)))
