#!/usr/bin/csi -s

(load "cp")

(define (celsius-fahrenheit C F)
  (c= (c* (cv 9) C)
      (c* (cv 5) (c- F (cv 32))))
  'ok)

(define C (make-connector))
(define F (make-connector))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(celsius-fahrenheit C F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)
