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
  (let ((input (read)))
    (if (eof-object? input)
        'done
        (let ((stmts (statements (compile input 'val 'next '()))))
          (for-each print-statement stmts)
          (compile-loop)))))

(compile-loop)
