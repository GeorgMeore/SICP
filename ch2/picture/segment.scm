(load "vect")

(define (make-segment start end)
  (cons start end))

(define (segment-start s)
  (car s))

(define (segment-end s)
  (cdr s))

(define (vectors->segments vectors)
  (if (< (length vectors) 2)
      '()
      (cons (make-segment (car vectors) (cadr vectors))
            (vectors->segments (cdr vectors)))))
