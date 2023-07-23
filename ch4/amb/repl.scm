#!/usr/bin/csi -s

(include "syntax.scm")
(include "internals.scm")
(include "evaluator.scm")


(define the-global-environment (setup-environment))

(define (display-object object)
  (if (compound-procedure? object)
      (display (list 'procedure
                     (procedure-parameters object)))
      (display object)))

(define (repl)
  (define (eval-loop retry)
    (display "amb> ")
    (let ((input (read)))
      (cond ((eq? input 'retry)
              (retry))
            ((not (eof-object? input))
              (evaluate
                input
                the-global-environment
                (lambda (value value-fail)
                  (display-object value)
                  (newline)
                  (eval-loop value-fail))
                (lambda ()
                  (display "amb: no more values\n")
                  (repl)))))))
  (eval-loop
    (lambda ()
      (display "amb: no evaluation in progress\n")
      (repl))))

(repl)
