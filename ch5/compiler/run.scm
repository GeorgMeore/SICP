#!/usr/bin/csi -s

(include "syntax.scm")
(include "instructions.scm")
(include "compiler.scm")

(define (print-statement stmt)
  (when (pair? stmt)
    (display "  "))
  (display stmt)
  (newline))

(for-each
  print-statement
  (statements
    (compile
      '(def (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))
      'val
      'next)))
