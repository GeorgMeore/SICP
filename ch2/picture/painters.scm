(load "segment")
(load "vect")
(load "utils")
(load "draw")

(define blank
	(lambda (frame) (void))
)

(define (segments->painter segment-list)
	(lambda (frame)
		(let ((fmap (frame-map frame)))
			(for-each
				(lambda (segment)
					(draw-line
						(fmap (segment-start segment))
						(fmap (segment-end segment))
					)
				)
				segment-list
			)
		)
	)
)

(define (vectors->painter vectors)
	(segments->painter (vectors->segments vectors))
)

(define (function->painter func step)
	(vectors->painter
		(map
			(lambda (x) (make-vect x (func x)))
			(seq 0 1 step)
		)
	)
)

(define outline
	(segments->painter
		(list
			(make-segment (make-vect 0 0) (make-vect 0 1))
			(make-segment (make-vect 0 1) (make-vect 1 1))
			(make-segment (make-vect 1 1) (make-vect 1 0))
			(make-segment (make-vect 1 0) (make-vect 0 0))
		)
	)
)

(define cross
	(segments->painter
		(list
			(make-segment (make-vect 0 0) (make-vect 1 1))
			(make-segment (make-vect 0 1) (make-vect 1 0))
		)
	)
)

(define diamond
	(segments->painter
		(list
			(make-segment (make-vect 1/2 0) (make-vect 1 1/2))
			(make-segment (make-vect 1 1/2) (make-vect 1/2 1))
			(make-segment (make-vect 1/2 1) (make-vect 0 1/2))
			(make-segment (make-vect 0 1/2) (make-vect 1/2 0))
		)
	)
)
