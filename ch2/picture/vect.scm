(define (make-vect x y) (cons x y))
(define (vect-x v) (car v))
(define (vect-y v) (cdr v))

(define (add-vect v1 v2)
	(make-vect
		(+ (vect-x v1) (vect-x v2))
		(+ (vect-y v1) (vect-y v2))
	)
)

(define (sub-vect v1 v2)
	(make-vect
		(- (vect-x v1) (vect-x v2))
		(- (vect-y v1) (vect-y v2))
	)
)

(define (scale-vect s v)
	(make-vect
		(* s (vect-x v))
		(* s (vect-y v))
	)
)
