(def (factorial x)
  (def (iter n acc)
    (if (> n x)
        acc
        (iter (+ n 1) (* acc n))))
  (iter 1 1))

(println (factorial 10))
