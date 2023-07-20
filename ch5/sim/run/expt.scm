#!/usr/bin/csi -s

(include "syntax.scm")
(include "assembler.scm")
(include "machine.scm")

(define expt
  (make-machine
    (list (cons '- -) (cons '* *) (cons '= =))
    '(controller
        (assign continue (label exit))
      expt
        (test (op =) (reg n) (const 0))
        (branch (label expt-base-case))
        (save continue)
        (assign continue (label expt-after-rec-call))
        (assign n (op -) (reg n) (const 1))
        (goto (label expt))
      expt-after-rec-call
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      expt-base-case
        (assign val (const 1))
        (goto (reg continue))
      exit)
  ))

(set-register-contents! expt 'b 3)
(set-register-contents! expt 'n 5)
(start expt)
(display (get-register-contents expt 'val))
(newline)
