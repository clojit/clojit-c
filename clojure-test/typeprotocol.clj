(do
  (defprotocol IBar
    (total [self]
      [self a]))
  (deftype Bar [a]
    IBar
    (total [self] 993)                                      ; 12820
    (total [self a] (+ 993 a)))                             ; 12821

  (total (->Bar 1)))