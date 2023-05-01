(load "syntax.scm")
(load "internals.scm")


(define (eval exp env)
  (cond ((self-evaluating? exp)
          exp)
        ((variable? exp)
          (lookup-variable-value exp env))
        ((quoted? exp)
          (text-of-quotation exp))
        ((assignment? exp)
          (eval-assignment exp env))
        ((definition? exp)
          (eval-definition exp env))
        ((if? exp)
          (eval-if exp env))
        ((lambda? exp)
          (eval-lambda exp env))
        ((begin? exp)
          (eval-sequence (begin-actions exp) env))
        ((scope? exp) ; used for debugging
          env)
        ((application? exp)
          (eval-application exp env))
        (else
          (error "unknown expression type" exp))))

(define (eval-assignment exp env)
  (let ((value (eval (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp) value env)
    value))

(define (eval-definition exp env)
  (let ((value (eval (definition-value exp) env)))
    (define-variable! (definition-variable exp) value env)
    value))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-lambda exp env)
  (make-procedure
    (lambda-parameters exp)
    (lambda-body exp)
    env))

(define (eval-sequence exps env)
  (cond ((null? (cdr exps))
          (eval (car exps) env))
        (else
          (eval (car exps) env)
          (eval-sequence (cdr exps) env))))

(define (eval-application exp env)
  (apply (eval (operator exp) env)
         (map (lambda (op) (eval op env))
              (operands exp))))

(define (apply proc args)
  (cond ((primitive-procedure? proc)
          (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
          (eval-sequence
            (procedure-body proc)
            (extend-environment
              (procedure-parameters proc)
              args
              (procedure-environment proc))))
        (else
          (error "unknown procedure type" proc))))
