(do
  (def a "abc")
  (def b :ab)

((fn [z] z) 
 ((fn [a] 
  (+ a 10)) 5)))
