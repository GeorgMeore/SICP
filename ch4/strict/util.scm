(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (zip a b)
  (if (or (null? a) (null? b))
      '()
      (cons (cons (car a) (car b))
            (zip (cdr a) (cdr b)))))
