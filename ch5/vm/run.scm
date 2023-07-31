#!/usr/bin/csi -s

(include "../sim/syntax.scm")
(include "../sim/assembler.scm")
(include "../sim/machine.scm")
(include "internals.scm")


(define the-global-environment (setup-environment))

(define evaluator-ops
  (list
    (cons 'list list)
    (cons 'cons
      ; need to wrap in a lambda because of some bug in the chicken compiler
      (lambda (x y) (cons x y)))
    (cons 'empty-arglist
      (lambda () '()))
    (cons 'adjoin-arg
      (lambda (arg arglist) (append arglist (list arg))))
    (cons 'get-global-environment
      (lambda () the-global-environment))
    (cons 'lookup-variable-value lookup-variable-value)
    (cons 'set-variable-value! set-variable-value!)
    (cons 'define-variable! define-variable!)
    (cons 'false? false?)
    (cons 'primitive-procedure? primitive-procedure?)
    (cons 'apply-primitive-procedure apply-primitive-procedure)
    (cons 'compiled-procedure? compiled-procedure?)
    (cons 'make-compiled-procedure make-compiled-procedure)
    (cons 'compiled-procedure-environment compiled-procedure-environment)
    (cons 'compiled-procedure-entry compiled-procedure-entry)
    (cons 'extend-environment extend-environment)
  ))

(define (read-text)
  (let ((inst (read)))
    (if (eof-object? inst)
        '()
        (cons inst (read-text)))))

(let ((evaluator (make-machine evaluator-ops (read-text))))
  (set-register-contents! evaluator 'env the-global-environment)
  (start evaluator))
