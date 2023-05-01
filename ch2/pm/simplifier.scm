(load "rules")
(load "instantiate")
(load "match")

(define (make-simplifier rules)
  (define (simplify exp)
    (try-rules
      (if (pair? exp)
          (map simplify exp)
          exp)
      rules))
  (define (try-rules exp rules)
    (if (null? rules)
        exp
        (let ((dict (match (rule-pattern (car rules)) exp (make-dict))))
          (if (eq? dict 'failed)
              (try-rules exp (cdr rules))
              (simplify (instantiate (rule-skeleton (car rules)) dict))))))
  simplify)
