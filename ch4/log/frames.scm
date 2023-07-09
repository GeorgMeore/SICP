(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (frame-lookup variable frame)
  (assoc variable frame))

(define (frame-extend variable value frame)
  (cons (make-binding variable value) frame))
