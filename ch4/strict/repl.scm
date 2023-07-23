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
  (display "strict> ")
  (let ((input (read)))
    (if (not (eof-object? input))
        (let ((output (evaluate input the-global-environment)))
          (display-object output)
          (newline)
          (repl)))))

(repl)
