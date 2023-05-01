(load "constraints")

(define (c+ x y)
  (let ((sum (make-connector)))
    (adder x y sum)
    sum))

(define (c- x y)
  (let ((diff (make-connector)))
    (adder y diff x)
    diff))

(define (c* x y)
  (let ((prod (make-connector)))
    (multiplier x y prod)
    prod))

(define (c/ x y)
  (let ((mult (make-connector)))
    (multiplier mult y x)
    mult))

(define (cv v)
  (let ((const (make-connector)))
    (constant v const)
    const))

(define (c= x y)
  (equality x y))
