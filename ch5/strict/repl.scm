#!/usr/bin/csi -s

(include "../sim/syntax.scm")
(include "../sim/assembler.scm")
(include "../sim/machine.scm")
(include "syntax.scm")
(include "internals.scm")


(define the-global-environment (setup-environment))

(define evaluator-ops
  (list
    ; syntax
    (cons 'self-evaluating? self-evaluating?)
    (cons 'variable? variable?)
    (cons 'quoted? quoted?)
    (cons 'text-of-quotation text-of-quotation)
    (cons 'lambda? lambda?)
    (cons 'lambda-parameters lambda-parameters)
    (cons 'lambda-body lambda-body)
    (cons 'assignment? assignment?)
    (cons 'assignment-variable assignment-variable)
    (cons 'assignment-value assignment-value)
    (cons 'definition? definition?)
    (cons 'definition-variable definition-variable)
    (cons 'definition-value definition-value)
    (cons 'if? if?)
    (cons 'if-predicate if-predicate)
    (cons 'if-consequent if-consequent)
    (cons 'if-alternative if-alternative)
    (cons 'begin? begin?)
    (cons 'begin-actions begin-actions)
    (cons 'last-exp? last-exp?)
    (cons 'first-exp first-exp)
    (cons 'rest-exps rest-exps)
    (cons 'application? application?)
    (cons 'operator operator)
    (cons 'operands operands)
    (cons 'no-operands? no-operands?)
    (cons 'first-operand first-operand)
    (cons 'rest-operands rest-operands)
    (cons 'last-operand? last-operand?)
    ; internals
    (cons 'op-failed? op-failed?)
    (cons 'clear-error! clear-error!)
    (cons 'set-error! set-error!)
    (cons 'empty-arglist
      (lambda () '()))
    (cons 'adjoin-arg
      (lambda (arg arglist) (append arglist (list arg))))
    (cons 'get-global-environment
      (lambda () the-global-environment))
    (cons 'lookup-variable-value lookup-variable-value)
    (cons 'set-variable-value! set-variable-value!)
    (cons 'define-variable! define-variable!)
    (cons 'true? true?)
    (cons 'primitive-procedure? primitive-procedure?)
    (cons 'apply-primitive-procedure apply-primitive-procedure)
    (cons 'compound-procedure? compound-procedure?)
    (cons 'make-procedure make-procedure)
    (cons 'procedure-parameters procedure-parameters)
    (cons 'procedure-parameters procedure-parameters)
    (cons 'procedure-environment procedure-environment)
    (cons 'procedure-body procedure-body)
    (cons 'extend-environment extend-environment)
    ; io
    (cons 'read read)
    (cons 'eof-object? eof-object?)
    (cons 'prompt
      (lambda () (display "ec> ")))
    (cons 'print-error print-error)
    (cons 'print-object print-object)
  ))

(define evaluator-text '(
  read-eval-print-loop
    (perform (op stack-reset))
    (perform (op prompt))
    (assign exp (op read))
    (test (op eof-object?) (reg exp))
    (branch (label exit))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))
  print-result
    (perform (op print-object) (reg val))
    (goto (label read-eval-print-loop))
  eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))
  ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
  ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (test (op op-failed?))
    (branch (label signal-error))
    (goto (reg continue))
  ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
  ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-do))
    (goto (label eval-dispatch))
  ev-assignment-do
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (test (op op-failed?))
    (branch (label signal-error))
    (assign val (const ok))
    (goto (reg continue))
  ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-do))
    (goto (label eval-dispatch))
  ev-definition-do
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))
  ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))
  ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))
  ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
  ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))
  ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))
  ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))
  ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
  ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
  ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))
  ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))
  ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
  ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))
  ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))
  ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))
  ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))
  apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label appl-primitive))
    (test (op compound-procedure?) (reg proc))
    (branch (label appl-compound))
    (goto (label unknown-procedure-type))
  appl-primitive
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (test (op op-failed?))
    (branch (label signal-error))
    (restore continue)
    (goto (reg continue))
  appl-compound
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (test (op op-failed?))
    (branch (label signal-error))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))
  unknown-expression-type
    (perform (op set-error!) (const "Unknown expression type"))
    (goto (label signal-error))
  unknown-procedure-type
    (restore continue)
    (perform (op set-error!) (const "Unknown procedure type"))
    (goto (label signal-error))
  signal-error
    (perform (op print-error))
    (perform (op clear-error!))
    (goto (label read-eval-print-loop))
  exit
  ))

(let ((evaluator (make-machine evaluator-ops evaluator-text)))
  (start evaluator))
