(def (sgn x)
  (cond ((< x 0) -1)
        ((> x 0) 1)
        (else 0)))

(let ((a 1) (b -1))
  (println a " " (sgn a))
  (println b " " (sgn b)))
