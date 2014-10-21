(do (defn testfn [a]
      (fn [b]
        (+ a b)))
  ((testfn 10) 5))
