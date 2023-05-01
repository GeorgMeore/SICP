#!/usr/bin/csi -s

(load "eval")


(define the-global-environment (setup-environment))

(define (display-object object)
  (if (compound-procedure? object)
      (display (list 'procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<env>))
      (display object)))

(define (repl)
  (display "> ")
  (let ((input (read)))
  	(if (not (eq? input #!eof))
  	    (let ((output (eval input the-global-environment)))
  	      (display-object output)
  	      (newline)
  	      (repl)))))

(repl)
