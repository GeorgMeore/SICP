(load "agenda")
(load "primitives")

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (cond ((not (empty-agenda? the-agenda))
          ((first-agenda-item the-agenda))
          (delete-first-agenda-item! the-agenda)
          (propagate))))
