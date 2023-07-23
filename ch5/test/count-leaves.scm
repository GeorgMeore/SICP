#!/usr/bin/csi -s

(include "../sim/syntax.scm")
(include "../sim/assembler.scm")
(include "../sim/machine.scm")

(define count-leaves-a
  (make-machine
    (list (cons '+ +)
          (cons 'null? null?)
          (cons 'pair? pair?)
          (cons 'car car)
          (cons 'cdr cdr))
    '(start
        (assign continue (label exit))
      check-tree
        (test (op null?) (reg tree))
        (branch (label is-null))
        (test (op pair?) (reg tree))
        (branch (label is-cons))
        (goto (label is-atom))
      is-cons
        (save continue)
        (assign continue (label left-done))
        (save tree)
        (assign tree (op car) (reg tree))
        (goto (label check-tree))
      left-done
        (restore tree)
        (assign tree (op cdr) (reg tree))
        (save val)
        (assign continue (label right-done))
        (goto (label check-tree))
      right-done
        (restore temp)
        (assign val (op +) (reg val) (reg temp))
        (restore continue)
        (goto (reg continue))
      is-atom
        (assign val (const 1))
        (goto (reg continue))
      is-null
        (assign val (const 0))
        (goto (reg continue))
      exit)
  ))


(define count-leaves-b
  (make-machine
    (list (cons '+ +)
          (cons 'null? null?)
          (cons 'pair? pair?)
          (cons 'car car)
          (cons 'cdr cdr))
    '(start
        (assign val (const 0))
        (assign continue (label exit))
      check-tree
        (test (op null?) (reg tree))
        (branch (label is-null-or-atom))
        (test (op pair?) (reg tree))
        (branch (label is-cons))
        (assign val (op +) (reg val) (const 1))
      is-null-or-atom
        (goto (reg continue))
      is-cons
        (save continue)
        (assign continue (label right-done))
        (save tree)
        (assign tree (op car) (reg tree))
        (goto (label check-tree))
      right-done
        (restore tree)
        (assign tree (op cdr) (reg tree))
        (restore continue)
        (goto (label check-tree))
      exit)
  ))

(let ((count-leaves count-leaves-a))
  (set-register-contents! count-leaves 'tree '(1 () (4 . (5)) (3 7)))
  (start count-leaves)
  (println (get-register-contents count-leaves 'val)))
