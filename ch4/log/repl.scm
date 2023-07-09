#!/usr/bin/csi -s

(include "streams.scm")
(include "syntax.scm")
(include "database.scm")
(include "frames.scm")
(include "evaluator.scm")


(define (repl)
  (display "log> ")
  (let ((query (expand-question-marks (read))))
    (cond ((eof-object? query)
            'done)
          ((assertion? query)
            (add-rule-or-assertion! (assertion-body query))
            (repl))
          (else
            (reset-rule-counter!)
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate query
                               frame
                               contract-question-mark))
                (qeval query
                       (singleton-stream '()))))
            (repl)))))

(repl)
