#!/usr/bin/csi -s

(include "syntax.scm")
(include "instructions.scm")
(include "codegen.scm")

(define (print-statement stmt)
  (when (pair? stmt)
    (display "  "))
  (display stmt)
  (newline))

(define (compile-loop)
  (let ((exp (read)))
    (if (eof-object? exp)
        'done
        (let ((stmts (statements (compile-toplevel exp))))
          (for-each print-statement stmts)
          (compile-loop)))))

(compile-loop)
