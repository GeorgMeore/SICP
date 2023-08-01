#!/usr/bin/csi -s

(include "syntax.scm")
(include "instructions.scm")
(include "codegen.scm")

(define (write-statement stmt)
  (when (pair? stmt)
    (display "  "))
  (write stmt)
  (newline))

(define (read-text)
  (let ((inst (read)))
    (if (eof-object? inst)
        '()
        (cons inst (read-text)))))

(let ((text (read-text)))
  (if (not (null? text))
      (for-each
        write-statement
        (statements (compile-toplevel text)))))
