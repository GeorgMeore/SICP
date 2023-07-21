#!/usr/bin/csi -s

(include "connector.scm")
(include "constraints.scm")
(include "operators.scm")

(define (celsius-fahrenheit C F)
  (:= (:* (:v 9) C)
      (:* (:v 5) (:- F (:v 32))))
  'ok)

(define C (make-connector))
(define F (make-connector))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(celsius-fahrenheit C F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)
