(def (adder x)
  (lambda (y) (+ x y)))

(println ((adder 5) 6))
