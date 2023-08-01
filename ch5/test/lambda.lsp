(def f
  ((lambda (x y)
    (lambda (a b c d e)
      ((lambda (y z) (* x y z))
        (* a b c)
        (+ c d x))))
    3
    4))

(println (f 10 11 12 13 14))
