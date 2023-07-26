#!/usr/bin/csi -s

(include "syntax.scm")
(include "instructions.scm")
(include "compiler.scm")

;(compile
;  '(def (factorial n)
;     (if (= n 1)
;         1
;         (* (factorial (- n 1)) n)))
;  'val
;  'next)

(for-each
  (lambda (i) (display i) (newline))
  (statements
    (compile
      '(begin (+ 1 2 3) 4)
      'val
      'next)))
