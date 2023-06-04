(load "syntax.scm")
(load "internals.scm")

(define (evaluate exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
          (analyze-self-evaluating exp))
        ((variable? exp)
          (analyze-variable exp))
        ((quoted? exp)
          (analyze-quoted exp))
        ((assignment? exp)
          (analyze-assignment exp))
        ((permanent-assignment? exp)
          (analyze-permanent-assignment exp))
        ((definition? exp)
          (analyze-definition exp))
        ((if? exp)
          (analyze-if exp))
        ((try? exp)
          (analyze-try exp))
        ((amb? exp)
          (analyze-amb exp))
        ((require? exp)
          (analyze-require exp))
        ((lambda? exp)
          (analyze-lambda exp))
        ((begin? exp)
          (analyze-sequence (begin-actions exp)))
        ((let? exp)
          (analyze (let-expression exp)))
        ((application? exp)
          (analyze-application exp))
        (else
          (error "unknown expression type" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-quoted exp)
  (let ((text (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed text fail))))

(define (analyze-assignment exp)
  (let ((name (assignment-variable exp))
        (executor (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (executor env
        (lambda (value value-fail)
          (let ((old-value (lookup-variable-value name env)))
            (set-variable-value! name value env)
            (succeed value
              (lambda ()
                (set-variable-value! name old-value env)
                (value-fail)))))
        fail))))

(define (analyze-permanent-assignment exp)
  (let ((name (permanent-assignment-variable exp))
        (executor (analyze (permanent-assignment-value exp))))
    (lambda (env succeed fail)
      (executor env
        (lambda (value value-fail)
          (set-variable-value! name value env)
          (succeed value value-fail))
        fail))))

(define (analyze-definition exp)
  (let ((name (definition-variable exp))
        (executor (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (executor env
        (lambda (value value-fail)
          (cond ((variable-is-defined? name env)
                  (let ((old-value (lookup-variable-value name env)))
                    (set-variable-value! name value env)
                    (succeed value
                      (lambda ()
                        (set-variable-value! name old-value env)
                        (value-fail)))))
                (else
                  (define-variable! name value env)
                  (succeed value
                    (lambda ()
                      (undefine-variable! name env)
                      (value-fail))))))
        fail))))

(define (analyze-if exp)
  (let ((predicate-executor (analyze (if-predicate exp)))
        (consequent-executor (analyze (if-consequent exp)))
        (alternative-executor (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (predicate-executor env
        (lambda (pred-value pred-fail)
          (if (true? pred-value)
              (consequent-executor env succeed pred-fail)
              (alternative-executor env succeed pred-fail)))
        fail))))

(define (analyze-try exp)
  (let ((first-executor (analyze (try-first exp)))
        (second-executor (analyze (try-second exp))))
    (lambda (env succeed fail)
      (first-executor env
        (lambda (first-value first-fail)
          (succeed first-value fail))
        (lambda ()
          (second-executor env succeed fail))))))

(define (analyze-amb exp)
  (define (try-choices choices env succeed fail)
    (if (null? choices)
        (fail)
        ((car choices) env
          succeed
          (lambda () (try-choices (cdr choices) env succeed fail)))))
  (let ((executors (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (try-choices executors env succeed fail))))

(define (analyze-require exp)
  (let ((predicate-executor (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (predicate-executor env
        (lambda (pred-value pred-fail)
          (if (true? pred-value)
              (succeed pred-value pred-fail)
              (pred-fail)))
        fail))))

(define (analyze-lambda exp)
  (let ((params (lambda-parameters exp))
        (body-executor (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure params body-executor env) fail))))

(define (analyze-sequence exps)
  (define (combine-executors executors)
    (if (null? (cdr executors))
        (car executors)
        (let ((first (car executors))
              (rest (combine-executors (cdr executors))))
          (lambda (env succeed fail)
            (first env
              (lambda (first-value first-fail)
                (rest env succeed first-fail))
              fail)))))
  (let ((executors (map analyze exps)))
    (if (null? executors)
        (error "empty sequence")
        (combine-executors executors))))

(define (analyze-application exp)
  (define (get-args executors env succeed fail)
    (if (null? executors)
        (succeed '() fail)
        ((car executors) env
          (lambda (first-value first-fail)
            (get-args
              (cdr executors)
              env
              (lambda (rest-values args-fail)
                (succeed (cons first-value rest-values) args-fail))
              first-fail))
          fail)))
  (let ((function-executor (analyze (operator exp)))
        (argument-executors (map analyze (operands exp))))
    (lambda (env succeed fail)
      (function-executor env
        (lambda (func func-fail)
          (get-args argument-executors env
            (lambda (args args-fail)
              (execute-application func args succeed args-fail))
            func-fail))
        fail))))

(define (execute-application func args succeed fail)
  (cond ((primitive-procedure? func)
          (succeed (apply-primitive-procedure func args)
                   fail))
        ((compound-procedure? func)
          ((procedure-executor func)
            (extend-environment
              (procedure-parameters func)
              args
              (procedure-environment func))
            succeed
            fail))
        (else
          (error "unknown procedure type" func))))
