(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin f) (car f))
(define (frame-edge1 f) (cadr f))
(define (frame-edge2 f) (cddr f))

(define (frame-map f)
  (lambda (v)
    (add-vect (frame-origin f)
              (add-vect (scale-vect (vect-x v) (frame-edge1 f))
                        (scale-vect (vect-y v) (frame-edge2 f))))))

(define (transform-frame frame new-origin new-edge1 new-edge2)
  (let ((fmap (frame-map frame))
        (old-origin (frame-origin frame)))
    (make-frame (fmap new-origin)
                (sub-vect (fmap new-edge1) old-origin)
                (sub-vect (fmap new-edge2) old-origin))))
