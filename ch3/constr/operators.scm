(define (:+ x y)
  (let ((sum (make-connector)))
    (adder x y sum)
    sum))

(define (:- x y)
  (let ((diff (make-connector)))
    (adder y diff x)
    diff))

(define (:* x y)
  (let ((prod (make-connector)))
    (multiplier x y prod)
    prod))

(define (:/ x y)
  (let ((mult (make-connector)))
    (multiplier mult y x)
    mult))

(define (:v v)
  (let ((const (make-connector)))
    (constant v const)
    const))

(define (:= x y)
  (equality x y))
