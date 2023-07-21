(define (rule-pattern rule)
  (car rule))

(define (rule-skeleton rule)
  (cadr rule))

(define (constant-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?c)))

(define (variable-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?v)))

(define (arbitrary-pattern? pat)
  (and (pair? pat) (eq? (car pat) '?)))

(define (pattern-variable pat)
  (cadr pat))

(define (skeleton-evaluation? skel)
  (and (pair? skel) (eq? (car skel) ':)))

(define (skeleton-expression skel)
  (cadr skel))

(define (constant? exp)
  (number? exp))

(define (variable? exp)
  (symbol? exp))
