(load "syntax.scm")
(load "internals.scm")


(define (evaluate exp env)
  (cond ((self-evaluating? exp)
          exp)
        ((variable? exp)
          (lookup-variable-value exp env))
        ((quoted? exp)
          (text-of-quotation exp))
        ((assignment? exp)
          (evaluate-assignment exp env))
        ((definition? exp)
          (evaluate-definition exp env))
        ((if? exp)
          (evaluate-if exp env))
        ((lambda? exp)
          (evaluate-lambda exp env))
        ((begin? exp)
          (evaluate-sequence (begin-actions exp) env))
        ((let? exp)
          (evaluate (let-expression exp) env))
        ((application? exp)
          (evaluate-application exp env))
        (else
          (error "unknown expression type" exp))))

(define (evaluate-assignment exp env)
  (let ((value (evaluate (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp) value env)
    value))

(define (evaluate-definition exp env)
  (let ((value (evaluate (definition-value exp) env)))
    (define-variable! (definition-variable exp) value env)
    value))

(define (evaluate-if exp env)
  (if (true? (evaluate (if-predicate exp) env))
      (evaluate (if-consequent exp) env)
      (evaluate (if-alternative exp) env)))

(define (evaluate-lambda exp env)
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))

(define (evaluate-sequence exps env)
  (cond ((null? (cdr exps))
          (evaluate (car exps) env))
        (else
          (evaluate (car exps) env)
          (evaluate-sequence (cdr exps) env))))

(define (evaluate-application exp env)
  (apply-procedure (evaluate (operator exp) env)
                   (map (lambda (op) (evaluate op env))
                        (operands exp))))

(define (apply-procedure proc args)
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
          (evaluate-sequence
            (procedure-body proc)
            (extend-environment
              (procedure-parameters proc)
              args
              (procedure-environment proc))))
        (else
          (error "unknown procedure type" proc))))
