#!/usr/bin/csi -s

(include "syntax.scm")
(include "machine.scm")
(include "assembler.scm")

(define fact
  (make-machine
    '(val n)
    (list (cons '- -) (cons '* *) (cons '= =))
    '(start
        (assign val (const 1))
      test-n
        (test (op =) (reg n) (const 0))
        (branch (label done))
        (assign val (op *) (reg n) (reg val))
        (assign n (op -) (reg n) (const 1))
        (goto (label test-n))
      done)
  ))

(set-register-contents! fact 'n 10)
(start fact)
(display (get-register-contents fact 'val))
(newline)
