(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))
