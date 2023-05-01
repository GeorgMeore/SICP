#!/usr/bin/csi -s

(load "simplifier")

; number representation:
; n = 0: ()
; n > 0: (1 (...)) where ... stands for the representation of n - 1
; n < 0: ((...) 1) where ... stands for the representation of n + 1
(define rules '(
  ; negation
  ( (neg ())         ()              )
  ( (neg (1 (? x)))  ((neg (: x)) 1) )
  ( (neg ((? x) 1))  (1 (neg (: x))) )
  ; addition
  ( (add (? x) ())             (: x)                     )
  ( (add (1 (? x)) (1 (? y)))  (add (1 (1 (: x))) (: y)) )
  ( (add ((? x) 1) ((? y) 1))  (add (((: x) 1) 1) (: y)) )
  ( (add (1 (? x)) ((? y) 1))  (add (: x) (: y))         )
  ( (add (? x) (? y))          (add (: y) (: x))         )
  ( (sub (? x) (? y))          (add (: x) (neg (: y)))   )
  ; multiplication
  ( (mul (? x) ())         ()                                        )
  ( (mul (? x) (1 (? y)))  (add (: x) (mul (: x) (: y)))             )
  ( (mul (? x) ((? y) 1))  (neg (add (: x) (mul (: x) (neg (: y))))) )
  ; comparison
  ( (gt (1 (? x)) ())  (1 ())                    )
  ( (gt (? x) ())      ()                        )
  ( (gt (? x) (? y))   (gt (sub (: x) (: y)) ()) )
  ( (eq () ())         (1 ())                    )
  ( (eq (? x) ())      ()                        )
  ( (eq (? x) (? y))   (eq (sub (: x) (: y)) ()) )
  ; conditional
  ( (if () (? t) (? f))     (: f) )
  ( (if (? c) (? t) (? f))  (: t) )
  ; factorial
  ( (fact ())         (1 ())                       )
  ( (fact (1 (? x)))  (mul (1 (: x)) (fact (: x))) )
))

(define simplify (make-simplifier rules))

(define (enc n)
  (cond
    ((= n 0) '())
    ((> n 0) (list 1 (enc (- n 1))))
    ((< n 0) (list (enc (+ n 1)) 1))
  )
)

(define (dec l)
  (cond
    ((null? l) 0)
    ((number? (car l)) (+ 1 (dec (cadr l))))
    ((number? (cadr l)) (- (dec (car l)) 1))
  )
)

(define program
  (list 'fact (enc 4))
)

(print
  (dec (simplify program))
)
