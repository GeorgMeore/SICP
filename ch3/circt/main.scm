#!/usr/bin/csi -s

(load "sim")

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define s (make-wire))

(define (inverted input)
  (let ((output (make-wire)))
    (inverter input output)
    output))

(define (half-adder a b s c)
  (let ((e (make-wire)) (d (make-wire)))
    (and-gate a b c)
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s)))

(probe 'a a)
(probe 'b b)
(probe 'c c)
(probe 's s)

(set-signal! a 1)
(set-signal! b 1)

(half-adder a b s c)

(propagate)
