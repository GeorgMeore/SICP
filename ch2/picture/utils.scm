(define (seq start stop step)
  (if (> start stop)
      '()
      (cons start (seq (+ start step) stop step))))

(define (repeat n x)
  (if (> n 0)
      (cons x (repeat (- n 1) x))
      '()))

(define (fmod a b)
  (- a (* b (floor (/ a b)))))

(define (radians deg)
  (* deg (/ (atan 1) 45)))
