(def (factorial n)
  (if (= n 0)
      1
      (* (factorial (- n 1)) n)))

(println (+ 1 1))

(println (factorial 10))
