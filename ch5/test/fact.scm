#!/usr/bin/csi -s

(include "../sim/syntax.scm")
(include "../sim/assembler.scm")
(include "../sim/machine.scm")

(define fact
  (make-machine
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
