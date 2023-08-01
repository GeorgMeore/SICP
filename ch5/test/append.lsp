(def (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(println (append '(1 2 3) '(4 5 6 7)))
