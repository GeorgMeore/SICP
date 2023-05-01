#!/usr/bin/csi -s

(load "vect")
(load "frame")
(load "transformers")
(load "painters")
(load "utils")

(define (paint . painters)
	(define main-frame
		(make-frame
			(make-vect 0 0)
			(make-vect 6 0)
			(make-vect 0 6)
		)
	)
	(define (do-paint painter)
		(start-image)
		(painter main-frame)
		(end-image)
	)
	(for-each do-paint painters)
)

(define (tile n)
	(overlay
		((square-of-four flip-horiz flip-diag identity flip-vert)
			(corner-split cross n)
		)
		outline
	)
)

(define (n-by-m painter n m)
	(let ((one-row (apply row (repeat m painter))))
		(apply column (repeat n one-row))
	)
)

(define (thing x)
	(overlay (rotate outline x) outline)
)

(define (stuff painter x n)
	(overlay
		outline
		(if (> n 0)
			(rotate (stuff painter x (- n 1)) x)
			painter
		)
	)
)

(define tick
	(vectors->painter
		(list
			(make-vect 1 1)
			(make-vect 1/2 1/2)
			(make-vect 1 0)
		)
	)
)

(paint
	(overlay
		(stuff cross 10 15)
		(stuff cross -10 15)
	)
	(tile 8)
	(n-by-m (tile 5) 5 5)
)
