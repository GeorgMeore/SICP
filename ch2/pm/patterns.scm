(define (constant-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?c)))

(define (variable-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?v)))

(define (arbitrary-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?)))

(define (pattern-variable pat)
  (cadr pat))


(define (constant? exp)
  (number? exp))

(define (variable? exp)
  (symbol? exp))
