(def (iter x)
  (println x)
  (iter (+ x 1)))

(iter 1)
