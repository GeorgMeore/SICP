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
  (statements (compile (read) 'val 'next)))
