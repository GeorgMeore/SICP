(def (range start)
  (cons start (lambda () (range (+ 1 start)))))

(def (filter p s)
  (let ((fst (car s))
        (rest (lambda () (filter p ((cdr s))))))
    (if (p fst) (cons fst rest) (rest))))

(def (sprint s)
  (println (car s))
  (sprint ((cdr s))))

(def (sieve l)
  (cons (car l)
        (lambda()
          (filter
           (lambda (x)
             (not (= (mod x (car l)) 0)))
           (sieve ((cdr l)))))))

(sprint (sieve (range 2)))
